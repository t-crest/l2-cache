package caches.hardware.reppol

import caches.hardware.util.Constants.CONTENTION_LIMIT_WIDTH
import caches.hardware.util.{MemBlock, PipelineReg, UpdateSingleVecElem}
import chisel3._
import chisel3.util._

class LineAssignmentsArray(nWays: Int, nSets: Int, nCores: Int) extends Module() {
  val io = IO(new Bundle {
    val stall = Input(Bool())
    val wrEn = Input(Bool())
    val rSet = Input(UInt(log2Up(nSets).W))
    val wrSet = Input(UInt(log2Up(nSets).W))
    val wrData = Input(Vec(nWays, UInt(log2Up(nCores).W)))
    val wrWay = Input(UInt(log2Up(nWays).W))
    val rLineAssign = Output(Vec(nWays, UInt(log2Up(nCores).W)))
    val rValidAssign = Output(Vec(nWays, Bool()))
  })

  // Memory array for keeping track of line assignments
  val lineAssignments = Module(new MemBlock(nSets, nWays * log2Up(nCores)))

  lineAssignments.io.wrEn := io.wrEn
  lineAssignments.io.readAddr := io.rSet
  lineAssignments.io.writeAddr := io.wrSet
  lineAssignments.io.writeData := io.wrData.asUInt

  // Valid line assignments register array
  val validLineAssignments = Array.fill(nSets)(RegInit(VecInit(Seq.fill(nWays)(false.B))))
  val rLineValidAssign = VecInit(Seq.fill(nWays)(false.B))

  val validRAddrDelayReg = PipelineReg(io.rSet, 0.U, io.stall)

  for (setIdx <- 0 until nSets) {
    when(setIdx.U === validRAddrDelayReg) {
      rLineValidAssign := validLineAssignments(setIdx)

      when(io.wrEn) {
        validLineAssignments(setIdx)(io.wrWay) := true.B
      }
    }
  }

  // Break apart the read data into a VEC
  val rLineAssignments = VecInit(Seq.fill(nWays)(0.U(log2Up(nCores).W)))
  for (idx <- 0 until nWays) {
    rLineAssignments(idx) := lineAssignments.io.readData((log2Up(nCores) - 1) + (idx * log2Up(nCores)), idx * log2Up(nCores))
  }

  io.rLineAssign := rLineAssignments
  io.rValidAssign := rLineValidAssign
}

class CoreContentionTable(nCores: Int) extends Module() {
  val io = IO(new Bundle{
    val schedCoreId = Input(Valid(UInt(log2Up(nCores).W)))
    val setCritical = Input(Bool())
    val unsetCritical = Input(Bool())
    val setContentionLimit = Input(UInt(CONTENTION_LIMIT_WIDTH.W))
    val incrContention = Input(Valid(UInt(log2Up(nCores).W)))
    val rLimits = Output(Vec(nCores, UInt(CONTENTION_LIMIT_WIDTH.W)))
    val rCritCores = Output(Vec(nCores, Bool()))
  })

  // Registers for keeping the state of each active core
  val contentionLimits = RegInit(VecInit(Seq.fill(nCores)(0.U(CONTENTION_LIMIT_WIDTH.W))))
  val criticalCores = RegInit(VecInit(Seq.fill(nCores)(false.B)))

  // Set and unset cores as critical
  for (coreTableIdx <- 0 until nCores) {
    when (io.setCritical && (io.schedCoreId.valid && io.schedCoreId.bits === coreTableIdx.U)) {
      criticalCores(coreTableIdx) := true.B
      contentionLimits(coreTableIdx) := io.setContentionLimit
    } .elsewhen (io.unsetCritical && (io.schedCoreId.valid && io.schedCoreId.bits === coreTableIdx.U)) {
      criticalCores(coreTableIdx) := false.B
      contentionLimits(coreTableIdx) := 0.U
    }

    when(io.incrContention.valid && io.incrContention.bits === coreTableIdx.U) {
      contentionLimits(coreTableIdx) := contentionLimits(coreTableIdx) - 1.U
    }
  }

  io.rLimits := contentionLimits
  io.rCritCores := criticalCores
}

/**
 * On a hit we update only the base policy, on an eviction we update the contention policy.
 *
 * @param nWays number of ways in a cache set
 * @param nSets number of sets in a cache
 * @param nCores number of cores sharing the cache
 * @param basePolicy the base replacement policy module generating function
 */
class ContentionReplacementPolicy(nWays: Int, nSets: Int, nCores: Int, basePolicy: () => SharedCacheReplacementPolicyType) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores) {
  // ---------------- Base policy stage ----------------

  // Base policy instantiation
  val basePolicyInst = Module(basePolicy())

  // Update base policy
  basePolicyInst.io.control.setIdx := io.control.setIdx
  basePolicyInst.io.control.coreId := io.control.coreId
  basePolicyInst.io.control.evict := io.control.evict
  basePolicyInst.io.control.update.valid := io.control.update.valid
  basePolicyInst.io.control.update.bits := io.control.update.bits
  basePolicyInst.io.control.stall := io.control.stall
  basePolicyInst.io.scheduler <> io.scheduler

  val setIdxPipeReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall)

  // ---------------- Eviction stage ----------------
  val contAlgorithm = Module(new ContentionReplacementAlgorithm(nWays, nCores))

  val assignArr = Module(new LineAssignmentsArray(nWays, nSets, nCores))
  assignArr.io.stall := io.control.stall
  assignArr.io.wrEn := io.control.evict
  assignArr.io.rSet := io.control.setIdx
  assignArr.io.wrSet := setIdxPipeReg
  assignArr.io.wrData := UpdateSingleVecElem(assignArr.io.rLineAssign, io.control.coreId, contAlgorithm.io.replacementWay.bits)
  assignArr.io.wrWay := contAlgorithm.io.replacementWay.bits

  val coreTable = Module(new CoreContentionTable(nCores))
  coreTable.io.schedCoreId := io.scheduler.coreId
  coreTable.io.setCritical := io.scheduler.setCritical
  coreTable.io.unsetCritical := io.scheduler.unsetCritical
  coreTable.io.setContentionLimit := io.scheduler.contentionLimit
  coreTable.io.incrContention := contAlgorithm.io.updateCore

  val isCoreCrit = coreTable.io.rCritCores(io.control.coreId)

  // Compute the eviction for each set
  contAlgorithm.io.evict := io.control.evict
  contAlgorithm.io.isReqCoreCritical := isCoreCrit
  contAlgorithm.io.baseCandidates := basePolicyInst.io.control.replacementSet
  contAlgorithm.io.lineAssignments := assignArr.io.rLineAssign
  contAlgorithm.io.validLineAssignments := assignArr.io.rValidAssign
  contAlgorithm.io.coreLimits := coreTable.io.rLimits
  contAlgorithm.io.criticalCores := coreTable.io.rCritCores

  io.control.replaceWay := contAlgorithm.io.replacementWay.bits
  io.control.isValid := contAlgorithm.io.replacementWay.valid
  io.control.replacementSet := VecInit(Seq.fill(nWays)(0.U(log2Up(nWays).W)))
}

object ContentionReplacementPolicy extends App {
//  val l2Size = 262144 // 256 KiB
//  val l2Size = 16384 // 16 KiB
  val l2Size = 131072 // 128 KiB
  val l2Ways = 8
  val nCores = 4
  val l2BytesPerBlock = 64
  val l2nSets = l2Size / (l2Ways * l2BytesPerBlock)

  val plruL2RepPolicy = () => new BitPlruReplacementPolicy(l2Ways, l2nSets, nCores)

  (new chisel3.stage.ChiselStage).emitVerilog(
    new ContentionReplacementPolicy(
      l2Ways,
      l2nSets,
      nCores,
      plruL2RepPolicy
    ),
    Array("--target-dir", "generated")
  )
}