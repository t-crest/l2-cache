package caches.hardware.pipelined

import chisel3._
import chisel3.util._
import caches.hardware.reppol._
import caches.hardware.pipelined.stages.{Dec, Read, Rep, Tag, UpdateUnit}

class CacheRequestIO(addrWidth: Int, dataWidth: Int, reqIdWidth: Int) extends Bundle {
  val reqId = Flipped(Decoupled(UInt(reqIdWidth.W)))
  val addr = Input(UInt(addrWidth.W))
  val rw = Input(Bool()) // 0 - Read, 1 - Write
  val byteEn = Input(UInt((dataWidth / 8).W))
  val wData = Input(UInt(dataWidth.W))
}

class CacheResponseIO(dataWidth: Int, reqIdWidth: Int) extends Bundle {
  val reqId = Valid(UInt(reqIdWidth.W))
  val rData = Output(UInt(dataWidth.W))
}

class CacheCorePortIO(addrWidth: Int, dataWidth: Int, reqIdWidth: Int) extends Bundle {
  val req = new CacheRequestIO(addrWidth, dataWidth, reqIdWidth)
  val resp = new CacheResponseIO(dataWidth, reqIdWidth)
}

/**
 * @param memBeatSize beat size in bytes
 * @param memBurstLen number of beats in a burst
 */
class SharedPipelinedCache(
                            sizeInBytes: Int,
                            nWays: Int,
                            nCores: Int,
                            reqIdWidth: Int,
                            addressWidth: Int,
                            bytesPerBlock: Int,
                            bytesPerSubBlock: Int,
                            memBeatSize: Int,
                            memBurstLen: Int,
                            l2RepPolicy: () => SharedCacheReplacementPolicyType,
                            nMshrs: Option[Int] = None,
                            nHalfMissCmds: Option[Int] = None,
                            printInfo: Boolean = true
                          ) extends Module {
  require(isPow2(memBeatSize), "Number of bytes per beat must be a power of 2.")
  require(isPow2(bytesPerBlock), "Number of bytes per block must be a power of 2.")
  require(isPow2(bytesPerBlock / bytesPerSubBlock), "The remainder of bytes per block divided by bytes per sub-block must be a power of 2.")

  private val nSets = sizeInBytes / (nWays * bytesPerBlock)
  private val subBlocksPerBlock = bytesPerBlock / bytesPerSubBlock
  private val byteOffsetWidth = log2Up(bytesPerSubBlock)
  private val blockOffsetWidth = log2Up(subBlocksPerBlock)
  private val indexWidth = log2Up(nSets)
  private val tagWidth = addressWidth - indexWidth - blockOffsetWidth - byteOffsetWidth

  private val mshrCnt = nMshrs match {
    case Some(v) => v
    case None => nWays / 2
  }

  private val halfMissCmdCnt = nHalfMissCmds match {
    case Some(v) => v
    case None => nWays
  }

  val repPol = Module(l2RepPolicy())
  val missQueue = Module(new MissFifo(nCores, halfMissCmdCnt, mshrCnt, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, bytesPerSubBlock * 8, bytesPerBlock * 8, enCritMisses = repPol.includeCriticalMissQ()))
  val wbQueue = Module(new WriteBackFifo(mshrCnt, tagWidth, indexWidth, bytesPerBlock * 8, enCritWb = repPol.includeCriticalWbQ()))
  val updateLogic = Module(new UpdateUnit(nCores, nWays, reqIdWidth, tagWidth, indexWidth, bytesPerBlock * 8, bytesPerSubBlock * 8))

  val schedulerDataWidth = repPol.getSchedulerDataWidth
  val l2CacheBytesPerSubBlock = bytesPerSubBlock

  val invalidateLine = WireDefault(false.B)
  val insertBubble = WireDefault(false.B)
  val invalidateWay = WireDefault(0.U(log2Up(nWays).W))
  val invalidateIndex = WireDefault(0.U(indexWidth.W))
  val missFifoCmdCapacity = WireDefault(false.B)
  val repDirtyInvalidStall = WireDefault(false.B)
  val writeMissHazard = WireDefault(false.B)

  if (printInfo) {
    println(
      s"L2 Cache Configuration: " +
        s"Size = $sizeInBytes bytes, " +
        s"Replacement policy = ${repPol.getClass.getSimpleName}, " +
        s"Associativity = $nWays, " +
        s"Block Size = $bytesPerBlock bytes, " +
        s"Sub-block Size = $bytesPerSubBlock bytes, " +
        s"Memory Beat Size = $memBeatSize bytes, " +
        s"Memory Burst Length = $memBurstLen beats, " +
        s"Miss Q depth = $mshrCnt, " +
        s"Miss Q half miss cmds per entry = $halfMissCmdCnt cmds, " +
        s"Number of Cores = $nCores"
    )

    repPol.printConfig() // Print configuration of replacement policy
  }

  val io = IO(new Bundle{
    val core = new CacheCorePortIO(addressWidth, bytesPerSubBlock * 8, reqIdWidth)
    val inCoreId = Input(UInt(log2Up(nCores).W))
    val outCoreId = Output(UInt(log2Up(nCores).W))
    val mem = new CacheMemoryControllerIO(addressWidth, memBeatSize)
    val scheduler = new SchedulerControlIO(nCores, schedulerDataWidth)
  })

  val rejectionQueue = Module(new RejectionQueue(nCores = nCores, addrWidth = addressWidth, dataWidth = bytesPerSubBlock * 8, reqIdWidth = reqIdWidth, depth = nCores))
  val coreReqArbiter = Module(new CoreReqArbiter(nCores = nCores, addrWidth = addressWidth, dataWidth = bytesPerSubBlock * 8, reqIdWidth = reqIdWidth))

  val pipeStall = updateLogic.io.stall || missQueue.io.full || missFifoCmdCapacity || repDirtyInvalidStall || writeMissHazard
  val reqAccept = !pipeStall && !insertBubble

  // Connect core request and rejection queue to the core request multiplexer that feeds into the cache pipeline
  coreReqArbiter.io.req1 <> io.core.req
  coreReqArbiter.io.req2 <> rejectionQueue.io.popEntry
  coreReqArbiter.io.req1CoreID := io.inCoreId
  coreReqArbiter.io.req2CoreID := rejectionQueue.io.popCoreId
  coreReqArbiter.io.out.reqId.ready := reqAccept

  // Connect replacement policy with the rejection queue
  repPol.io.scheduler <> io.scheduler
  insertBubble := repPol.io.control.insertBubble

  // ---------------- Decode ----------------
  val decLogic = Module(new Dec(nCores = nCores, nWays = nWays, reqIdWidth = reqIdWidth, tagWidth = tagWidth, indexWidth = indexWidth, blockOffWidth = blockOffsetWidth, byteOffWidth = byteOffsetWidth, subBlockWidth = bytesPerSubBlock * 8))
  decLogic.io.stall := pipeStall || insertBubble
  decLogic.io.dec.coreId := coreReqArbiter.io.outCoreID
  decLogic.io.dec.reqValid := coreReqArbiter.io.out.reqId.valid
  decLogic.io.dec.reqId := coreReqArbiter.io.out.reqId.bits
  decLogic.io.dec.reqRw := coreReqArbiter.io.out.rw
  decLogic.io.dec.addr := coreReqArbiter.io.out.addr
  decLogic.io.dec.wData := coreReqArbiter.io.out.wData
  decLogic.io.dec.byteEn := coreReqArbiter.io.out.byteEn

  // ---------------- Tag and Dirty Lookup ----------------
  val tagLogic = Module(new Tag(nCores = nCores, nSets = nSets, nWays = nWays, reqIdWidth = reqIdWidth, tagWidth = tagWidth, indexWidth = indexWidth, blockOffWidth = blockOffsetWidth, subBlockWidth = bytesPerSubBlock * 8))
  tagLogic.io.stall := pipeStall || insertBubble
  tagLogic.io.tag <> decLogic.io.tag
  tagLogic.io.tagCtrl <> updateLogic.io.tagUpdate
  tagLogic.io.invalidate.invalidate := invalidateLine
  tagLogic.io.invalidate.way := invalidateWay
  tagLogic.io.invalidate.index := invalidateIndex
  tagLogic.io.setLineValid := updateLogic.io.setValidLine

  // ---------------- Replacement ----------------
  val repLogic = Module(new Rep(nCores = nCores, nSets = nSets, nWays = nWays, nMshrs = mshrCnt, reqIdWidth = reqIdWidth, tagWidth = tagWidth, indexWidth = indexWidth, blockWidth = bytesPerBlock * 8, subBlockWidth = bytesPerSubBlock * 8, useInvalidate = !repPol.isInstanceOf[ContentionReplacementPolicy]))
  repLogic.io.stall := pipeStall
  repLogic.io.rep <> tagLogic.io.rep
  repLogic.io.missFifoPush <> missQueue.io.push
  repLogic.io.missCritInfo <> missQueue.io.critInfo
  repLogic.io.missNonCritInfo <> missQueue.io.nonCritInfo
  repLogic.io.repPolCtrl <> repPol.io.control
  repLogic.io.repPolInfo <> repPol.io.info
  repLogic.io.setLineValid := updateLogic.io.setValidLine
  invalidateLine := repLogic.io.invalidate.invalidate
  invalidateWay := repLogic.io.invalidate.way
  invalidateIndex := repLogic.io.invalidate.index
  missQueue.io.pushCrit := repLogic.io.isMissPushCrit
  missFifoCmdCapacity := repLogic.io.halfMissCapacity
  repDirtyInvalidStall := repLogic.io.evictionLineBusy
  rejectionQueue.io.push := repLogic.io.pushReject
  rejectionQueue.io.pushEntry := repLogic.io.pushRejectEntry
  repLogic.io.wbInfo <> wbQueue.io.wbInfo

  // ---------------- Read ----------------
  val readLogic = Module(new Read(memSizeInBytes = sizeInBytes, nCores = nCores, nWays = nWays, reqIdWidth = reqIdWidth, tagWidth = tagWidth, indexWidth = indexWidth, blockOffWidth = blockOffsetWidth, blockWidth = bytesPerBlock * 8, subBlockWidth = bytesPerSubBlock * 8))
  readLogic.io.stall := pipeStall
  readLogic.io.read <> repLogic.io.read
  readLogic.io.wbQueue <> wbQueue.io.push
  readLogic.io.memUpdate <> updateLogic.io.memUpdate
  tagLogic.io.dirtyCtrl := readLogic.io.dirtyCtrl
  repLogic.io.dirtyCtrl := readLogic.io.dirtyCtrl
  wbQueue.io.pushCrit := readLogic.io.wbQueuePushCrit

  // ---------------- Update ----------------
  val memInterface = Module(new MemoryInterface(nCores, nWays, halfMissCmdCnt, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, bytesPerBlock * 8, bytesPerSubBlock * 8, beatSize = memBeatSize, burstLen = memBurstLen))
  memInterface.io.missFifo <> missQueue.io.pop
  memInterface.io.missCritEmpty := missQueue.io.critEmpty
  memInterface.io.missNonCritEmpty := missQueue.io.nonCritEmpty
  memInterface.io.wbFifo <> wbQueue.io.pop
  memInterface.io.wbCritEmpty := wbQueue.io.critEmpty
  memInterface.io.wbNonCritEmpty := wbQueue.io.nonCritEmpty
  memInterface.io.memController <> io.mem
  missQueue.io.popQSel := memInterface.io.popQSel
  wbQueue.io.popQSel := memInterface.io.popQSel

  updateLogic.io.readStage <> readLogic.io.update
  updateLogic.io.memoryInterface <> memInterface.io.updateLogic
  updateLogic.io.pipeStall := pipeStall
  io.core.resp <> updateLogic.io.coreResp
  io.outCoreId := updateLogic.io.outCoreId
}
