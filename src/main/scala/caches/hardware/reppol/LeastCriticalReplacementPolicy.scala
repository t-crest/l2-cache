package caches.hardware.reppol

import caches.hardware.util.PipelineReg
import chisel3._
import chisel3.util._

/**
 * Least Critical (LC) replacement policy, after Kumar et al., "Cache design for mixed criticality real-time systems",
 * ICCD 2014, but with criticality per core instead of per address range. Selected in Patmos with repl="lc" in the
 * L2Cache XML element.
 *
 * A line is critical if it is assigned to a core that is currently set as critical. A line is assigned to the core
 * that allocates it, and, if claimOnHit is set, to a critical core that hits a non-critical line (the line now holds
 * data used by a critical core). On a miss, the first non-critical line in the base policy order is evicted. If all
 * lines in the set are critical, the first line in the base policy order is evicted, regardless of the requesting
 * core. Unlike the contention policy, no priority queues are used.
 *
 * With rejectNonCritical set, a miss of a non-critical core to a set in which all lines are critical is rejected
 * instead, and retried later through the rejection queue. A critical core still evicts the first line.
 *
 * The scheduler interface is compatible with the contention policy: a write sets the core as critical (the written
 * value is ignored) and a read unsets it, returning 1 if the core was critical. Reset unsets all cores.
 *
 * Pipeline (same as the other policies, each stage is one cycle):
 *  - Read stage:   the base policy state and the line assignments of the requested set are read from memory.
 *  - Update stage: the replacement candidates are known; select the way to replace and compute the new state.
 *  - WB stage:     io.control.update/evict tell whether the request hit (update) or allocated a line (evict);
 *                  the new state and line assignment are written back and forwarded to younger requests.
 *
 * @param nWays              number of ways in a single cache set
 * @param nSets              number of sets in the whole cache
 * @param nCores             number of cores sharing the cache
 * @param basePolicyType     base policy that orders the replacement candidates
 * @param claimOnHit         assign a non-critical line to a critical core when the critical core hits it
 * @param rejectNonCritical  reject a non-critical miss when all lines in the set are critical
 * @param schedulerDataWidth width of the data written and read through the scheduler interface
 * @param missQueueDepth     number of entries in the miss queue
 * @param repSetFormat       replacement set representation format, only the numerical format is supported
 */
class LeastCriticalReplacementPolicy(
                               nWays: Int,
                               nSets: Int,
                               nCores: Int,
                               basePolicyType: BasePolicyType = BasePolicies.BIT_PLRU,
                               claimOnHit: Boolean = true,
                               rejectNonCritical: Boolean = false,
                               schedulerDataWidth: Int = 32,
                               missQueueDepth: Int = 4,
                               repSetFormat: BaseReplacementSetFormat = new NumericalFormat
                             ) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores, schedulerDataWidth, missQueueDepth, repSetFormat) {
  require(repSetFormat == NumericalFormat(), "The least critical policy only supports the numerical replacement set format")

  override def printConfig(): Unit = println(s"Least critical replacement policy configuration: " +
    s"base policy type: ${basePolicyType.getName}, " +
    s"replacement set format: ${repSetFormat.getName}, " +
    s"claim on hit: $claimOnHit, " +
    s"reject non-critical: $rejectNonCritical, " +
    s"ways: $nWays, " +
    s"sets: $nSets, " +
    s"cores: $nCores.")

  // LC: replacement only, so no priority miss and write-back queues
  override def includeCriticalMissQ(): Boolean = false

  override def includeCriticalWbQ(): Boolean = false

  // ---------------- Critical cores ----------------
  val criticalCores = RegInit(VecInit(Seq.fill(nCores)(false.B)))

  // LC: the scheduler sets and clears critical cores, clearing a core releases all its lines
  io.scheduler.rData := 0.U
  switch(io.scheduler.cmd) {
    is(SchedulerCmd.WR) {
      criticalCores(io.scheduler.addr) := true.B
    }
    is(SchedulerCmd.RD) {
      criticalCores(io.scheduler.addr) := false.B
      io.scheduler.rData := criticalCores(io.scheduler.addr)
    }
    is(SchedulerCmd.RST) {
      criticalCores.foreach(_ := false.B)
    }
  }

  // ---------------- Read stage ----------------
  val basePolRead = Module(basePolicyType.buildBasePolicyRead(nWays, nSets, repSetFormat))
  val assignArr = Module(new LineAssignmentsArray(nWays, nSets, nCores))

  // WB stage signals, assigned below
  val wbStageIdx = WireDefault(0.U(log2Up(nSets).W))
  val wbStageState = WireDefault(0.U(basePolRead.stateWidth.W))
  val wbStageAssignWrEn = WireDefault(false.B)
  val wbStageAssignWay = WireDefault(0.U(log2Up(nWays).W))
  val wbStageAssignCore = WireDefault(0.U(log2Up(nCores).W))

  // Delayed by one cycle since the base policy state is stored in memory
  val idxDelayReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall)
  val fwd1 = wbStageIdx === idxDelayReg && io.control.update

  basePolRead.io.stall := io.control.stall
  basePolRead.io.rIdx := io.control.setIdx
  basePolRead.io.wrEn := io.control.update
  basePolRead.io.wIdx := wbStageIdx
  basePolRead.io.wData := wbStageState
  basePolRead.io.fwd := fwd1

  // The assignments are read one cycle later, so they arrive in the update stage. The memory forwards a write that
  // happens in the same cycle as the read, the write in the following cycle is forwarded in the update stage.
  assignArr.io.stall := io.control.stall
  assignArr.io.rSet := idxDelayReg
  assignArr.io.wrEn := wbStageAssignWrEn
  assignArr.io.wrSet := wbStageIdx
  assignArr.io.wrWay := wbStageAssignWay
  assignArr.io.wrLineAssign := wbStageAssignCore

  val statePipeReg1 = PipelineReg(basePolRead.io.readState, 0.U, !io.control.stall)
  val setIdxPipeReg1 = PipelineReg(idxDelayReg, 0.U, !io.control.stall)
  val coreIdPipeReg1 = PipelineReg(io.control.coreId, 0.U, !io.control.stall)

  // ---------------- Update stage ----------------
  val basePolUpdate = Module(basePolicyType.buildBasePolicyUpdate(nWays))

  val fwd2 = wbStageIdx === setIdxPipeReg1 && io.control.update
  val inState = WireDefault(0.U(basePolRead.stateWidth.W))
  inState := Mux(fwd2, wbStageState, statePipeReg1)
  // Derive the candidates from the forwarded state, so they are also correct for back-to-back requests to one set
  val baseCandidates = basePolRead.getNumericalReplacementSet(inState)

  // Line assignments, with the assignment written by the request in the WB stage forwarded
  val fwdAssign = wbStageAssignWrEn && wbStageIdx === setIdxPipeReg1
  val lineAssignments = Wire(Vec(nWays, UInt(log2Up(nCores).W)))
  val validLineAssignments = Wire(Vec(nWays, Bool()))
  val criticalWays = Wire(Vec(nWays, Bool()))
  for (wayIdx <- 0 until nWays) {
    val fwdWay = fwdAssign && wbStageAssignWay === wayIdx.U
    lineAssignments(wayIdx) := Mux(fwdWay, wbStageAssignCore, assignArr.io.rLineAssign(wayIdx))
    validLineAssignments(wayIdx) := fwdWay || assignArr.io.rValidAssign(wayIdx)
    // LC: a line is critical while it is assigned to a critical core
    criticalWays(wayIdx) := validLineAssignments(wayIdx) && criticalCores(lineAssignments(wayIdx))
  }

  // LC: evict the first non-critical way in base policy order, or the first way if all ways are critical
  val orderedNonCritical = VecInit(baseCandidates.map(way => !criticalWays(way)))
  val anyNonCritical = orderedNonCritical.asUInt.orR
  val repWay = Mux(anyNonCritical, baseCandidates(PriorityEncoder(orderedNonCritical)), baseCandidates(0))

  // LC: with rejection, a non-critical miss is rejected instead of evicting a critical way
  val isUpdateCoreCrit = criticalCores(coreIdPipeReg1)
  val reject = if (rejectNonCritical) !io.info.isHit && !anyNonCritical && !isUpdateCoreCrit else false.B

  basePolUpdate.io.hit := io.info.isHit
  basePolUpdate.io.hitWay := io.info.hitWay
  basePolUpdate.io.repWay := repWay
  basePolUpdate.io.stateIn := inState

  val repWayPipeReg2 = PipelineReg(repWay, 0.U, !io.control.stall)
  val isValidPipeReg2 = PipelineReg(!reject, true.B, !io.control.stall)
  val repSetPipeReg2 = PipelineReg(baseCandidates, getDefaultRepSet, !io.control.stall)
  val statePipeReg2 = PipelineReg(basePolUpdate.io.stateOut, 0.U, !io.control.stall)
  val setIdxPipeReg2 = PipelineReg(setIdxPipeReg1, 0.U, !io.control.stall)
  val coreIdPipeReg2 = PipelineReg(coreIdPipeReg1, 0.U, !io.control.stall)
  val isHitPipeReg2 = PipelineReg(io.info.isHit, false.B, !io.control.stall)
  val hitWayPipeReg2 = PipelineReg(io.info.hitWay, 0.U, !io.control.stall)
  val isHitWayCritPipeReg2 = PipelineReg(criticalWays(io.info.hitWay), false.B, !io.control.stall)

  // ---------------- WB stage ----------------
  // LC: an allocated line is assigned to the requesting core. A critical core claims a non-critical line it hits.
  val isReqCoreCrit = criticalCores(coreIdPipeReg2)
  val claim = if (claimOnHit) io.control.update && isHitPipeReg2 && isReqCoreCrit && !isHitWayCritPipeReg2 else false.B

  wbStageIdx := setIdxPipeReg2
  wbStageState := statePipeReg2
  wbStageAssignWrEn := io.control.evict || claim
  wbStageAssignWay := Mux(io.control.evict, repWayPipeReg2, hitWayPipeReg2)
  wbStageAssignCore := coreIdPipeReg2

  // Default output assignments
  io.control <> 0.U.asTypeOf(io.control)
  io.info <> 0.U.asTypeOf(io.info)

  io.control.replaceWay := repWayPipeReg2
  io.control.isValid := isValidPipeReg2 // LC: only rejected with rejectNonCritical, a rejected request leaves no state behind

  // Debugging signal
  repSet := repSetPipeReg2
}
