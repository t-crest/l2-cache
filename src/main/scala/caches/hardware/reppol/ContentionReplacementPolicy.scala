package caches.hardware.reppol

import caches.hardware.util.Constants.CONTENTION_LIMIT_WIDTH
import caches.hardware.util.{MemBlock, PipelineReg}
import chisel3._
import chisel3.util._

class LineAssignmentsArray(nWays: Int, nSets: Int, nCores: Int) extends Module() {
  val io = IO(new Bundle {
    val stall = Input(Bool())
    val rSet = Input(UInt(log2Up(nSets).W))
    val wrEn = Input(Bool())
    val wrSet = Input(UInt(log2Up(nSets).W))
    val wrWay = Input(UInt(log2Up(nWays).W))
    val wrLineAssign = Input(UInt(log2Up(nCores).W))
    val rLineAssign = Output(Vec(nWays, UInt(log2Up(nCores).W)))
    val rValidAssign = Output(Vec(nWays, Bool()))
  })

  // Memory array for keeping track of line assignments
  val lineAssignments = Array.fill(nWays)(Module(new MemBlock(nSets, log2Up(nCores))))
  val validLineAssignments = Array.fill(nWays)(Module(new MemBlock(nSets, 1)))

  val rLineAssignments = VecInit(Seq.fill(nWays)(0.U(log2Up(nCores).W)))
  val rValidLineAssignments = VecInit(Seq.fill(nWays)(false.B))

  for (wayIdx <- 0 until nWays) {
    val wrWayEn = io.wrWay === wayIdx.U

    lineAssignments(wayIdx).io.wrEn := io.wrEn && wrWayEn
    lineAssignments(wayIdx).io.readAddr := io.rSet
    lineAssignments(wayIdx).io.writeAddr := io.wrSet
    lineAssignments(wayIdx).io.writeData := io.wrLineAssign
    lineAssignments(wayIdx).io.stall := io.stall

    validLineAssignments(wayIdx).io.wrEn := io.wrEn && wrWayEn
    validLineAssignments(wayIdx).io.readAddr := io.rSet
    validLineAssignments(wayIdx).io.writeAddr := io.wrSet
    validLineAssignments(wayIdx).io.writeData := true.B
    validLineAssignments(wayIdx).io.stall := io.stall

    rLineAssignments(wayIdx) := lineAssignments(wayIdx).io.readData
    rValidLineAssignments(wayIdx) := validLineAssignments(wayIdx).io.readData
  }

  io.rLineAssign := rLineAssignments
  io.rValidAssign := rValidLineAssignments
}

class CoreContentionTable(nCores: Int) extends Module() {
  val io = IO(new Bundle {
    val stall = Input(Bool())
    val schedCoreId = Input(UInt(log2Up(nCores).W))
    val setCritical = Input(Bool())
    val unsetCritical = Input(Bool())
    val setContLimit = Input(UInt(CONTENTION_LIMIT_WIDTH.W))
    val wrEns = Input(Vec(2, Bool()))
    val wrCoreIds = Input(Vec(2, UInt(log2Up(nCores).W)))
    val wrCoreLimits = Input(Vec(2, UInt(CONTENTION_LIMIT_WIDTH.W)))
    val readData = Output(UInt(CONTENTION_LIMIT_WIDTH.W)) // Returns the current core limit if it is unset as critical to the scheduler
    val rLimits = Output(Vec(nCores, UInt(CONTENTION_LIMIT_WIDTH.W)))
    val rCritCores = Output(Vec(nCores, Bool()))
    val freeRejectionQueue = Output(Bool())
  })

  // NOTE: There is no forwarding here, if a core is set or unset as critical,
  // the current cache request will not see it until the next cycle.

  // Registers for keeping the state of each active core
  val contentionLimits = RegInit(VecInit(Seq.fill(nCores)(0.U(CONTENTION_LIMIT_WIDTH.W))))
  val criticalCores = RegInit(VecInit(Seq.fill(nCores)(false.B)))

  // Set and unset cores as critical
  for (coreTableIdx <- 0 until nCores) {
    // Store updated contention limits
    when(!io.stall) {
      when(io.wrEns(0) && coreTableIdx.U === io.wrCoreIds(0)) {
        contentionLimits(coreTableIdx) := io.wrCoreLimits(0)
      } .elsewhen(io.wrEns(1) && coreTableIdx.U === io.wrCoreIds(1)) {
        contentionLimits(coreTableIdx) := io.wrCoreLimits(1)
      }
    }

    // Scheduler assignments
    when(io.setCritical && io.schedCoreId === coreTableIdx.U) {
      criticalCores(coreTableIdx) := true.B
      contentionLimits(coreTableIdx) := io.setContLimit
    }.elsewhen(io.unsetCritical && io.schedCoreId === coreTableIdx.U) {
      criticalCores(coreTableIdx) := false.B
      contentionLimits(coreTableIdx) := 0.U
    }
  }

  // When a core is unset we empty the rejection queue
  val freeRejQueue = WireDefault(false.B)
  when(io.unsetCritical) {
    freeRejQueue := criticalCores(io.schedCoreId) // Free the rejection queue if the request was critical
  }

  val readData = WireDefault(0.U(CONTENTION_LIMIT_WIDTH.W))
  when(io.unsetCritical) {
    readData := contentionLimits(io.schedCoreId)
  }

  io.rLimits := contentionLimits
  io.rCritCores := criticalCores
  io.freeRejectionQueue := freeRejQueue
  io.readData := readData
}

class CoreLimitUpdateCtrl(nCores: Int, nWays: Int, nMshrs: Int, enaMim: Boolean = false, enaWb: Boolean = false, enaPrec: Boolean = false) extends Module() {
  val io = IO(new Bundle {
    val evict = Input(Bool())
    val evictionEvent = Input(Bool())
    val reqCore = Input(UInt(log2Up(nCores).W))
    val criticalCores = Input(Vec(nCores, Bool()))
    val hitWayIdx = Input(UInt(log2Up(nWays).W))
    val isHit = Input(Bool())
    val firstUcSetWayCore = Input(UInt(log2Up(nCores).W))
    val lineAssignments = Input(Vec(nWays, UInt(log2Up(nCores).W)))
    val validLineAssignments = Input(Vec(nWays, Bool()))
    val coreLimits = Input(Vec(nCores, UInt(CONTENTION_LIMIT_WIDTH.W)))
    val missQueueValidCores = Input(Vec(nMshrs, Bool()))
    val missQueueCritCores = Input(Vec(nMshrs, Bool()))
    val wbQueueValidCores = Input(Vec(nMshrs, Bool()))
    val wbQueueCritCores = Input(Vec(nMshrs, Bool()))
    val updtCore1 = Output(UInt(log2Up(nCores).W))
    val updtCore1Val = Output(UInt(CONTENTION_LIMIT_WIDTH.W))
    val updtCore2 = Output(UInt(log2Up(nCores).W))
    val updtCore2En = Output(Bool())
    val updtCore2Val = Output(UInt(CONTENTION_LIMIT_WIDTH.W))
  })

  val updtCore2En = WireDefault(false.B)
  val isReqCoreCritical = io.criticalCores(io.reqCore)
  val doesReqCoreOwnFirstUcWay = io.firstUcSetWayCore === io.reqCore

  val nonCritMisses = VecInit(Seq.tabulate(nMshrs)((i: Int) => io.missQueueValidCores(i) && !io.missQueueCritCores(i)))
  val nonCritWbs = VecInit(Seq.tabulate(nMshrs)((i: Int) => io.wbQueueValidCores(i) && !io.wbQueueCritCores(i)))

  // Decrement the contention limit when we encounter an eviction or replacement event
  val evictVal1 = WireDefault(0.U((CONTENTION_LIMIT_WIDTH + 1).W))
  val evictVal2 = WireDefault(0.U((CONTENTION_LIMIT_WIDTH + 1).W))
  // NOTE: No underflow can happen here since an eviction event is not triggered if the core limit is 0
  when (io.evict && io.evictionEvent) { // io.evict needed here
    when(doesReqCoreOwnFirstUcWay) {
      evictVal1 := io.coreLimits(io.reqCore) - 1.U
    } .otherwise {
      evictVal1 := io.coreLimits(io.reqCore)
      evictVal2 := io.coreLimits(io.firstUcSetWayCore) - 1.U
      updtCore2En := true.B
    }
  } .otherwise{
    evictVal1 := io.coreLimits(io.reqCore)
  }

  val precVal = WireDefault(0.U(CONTENTION_LIMIT_WIDTH.W))
  if (enaPrec) {
    // If the core whose way we hit is not owned by a critical core, then we increment the contention limit.
    // Additionally, we only increment it if the requesting core is critical.
    val ownerCore = io.lineAssignments(io.hitWayIdx)
    val ownerValid = io.validLineAssignments(io.hitWayIdx)
    val precedentEvent = isReqCoreCritical && !io.criticalCores(ownerCore) && ownerValid && io.isHit
    val incrEvictVal1 = WireDefault(0.U((CONTENTION_LIMIT_WIDTH + 1).W))
    incrEvictVal1 := evictVal1 + 1.U
    val overflow = incrEvictVal1(CONTENTION_LIMIT_WIDTH) === 1.U

    when(precedentEvent && !overflow) {
      precVal := incrEvictVal1
    }.otherwise {
      precVal := evictVal1
    }
  } else {
    precVal := evictVal1
  }

  // Trigger Miss-In-Miss and Miss-Q events
  val mimVal = WireDefault(0.U((log2Up(nMshrs) + 1).W))
  if (enaMim) {
    // We check if there are any non-critical misses ahead and if the requesting core is critical
    val missInMissEvent = isReqCoreCritical && nonCritMisses.reduce((x, y) => x || y)

    when(missInMissEvent) {
      mimVal := PopCount(nonCritMisses)
    }
  }

  // Writeback event
  val wbVal = WireDefault(0.U((log2Up(nMshrs) + 1).W))
  if (enaWb) {
    // We check if there are any non-critical WBs and if the requesting core is critical
    val wbEvent = isReqCoreCritical && nonCritWbs.reduce((x, y) => x || y)

    when(wbEvent) {
      wbVal := PopCount(nonCritWbs)
    }
  }

  val wbMimRes = (enaMim, enaWb) match {
    case (true, true) => wbVal + mimVal
    case (false, true) => wbVal
    case (true, false) => mimVal
    case (false, false) => 0.U
  }

  val evictValSubWbMimRes = WireDefault(0.U((CONTENTION_LIMIT_WIDTH + 1).W))
  evictValSubWbMimRes := evictVal1 - wbMimRes
  val underflow = evictValSubWbMimRes(CONTENTION_LIMIT_WIDTH)
  val evictWbMimRes = Mux(underflow, evictVal1, evictVal1 - wbMimRes)

  val reqCoreUpdtVal = Mux(io.evict, evictWbMimRes, precVal)

  io.updtCore1Val := reqCoreUpdtVal
  io.updtCore1 := io.reqCore
  io.updtCore2 := io.firstUcSetWayCore
  io.updtCore2En := updtCore2En
  io.updtCore2Val := evictVal2
}

/**
 * On a hit we update only the base policy, on an eviction we update the contention policy.
 */
class ContentionReplacementPolicy(
                                   nWays: Int,
                                   nSets: Int,
                                   nCores: Int,
                                   basePolicyType: BasePolicyType,
                                   enableMissInMiss: Boolean = false,
                                   enablePrecedentEvents: Boolean = false,
                                   enableWbEvents: Boolean = false,
                                   missQueueDepth: Int = 4,
                                   repSetFormat: BaseReplacementSetFormat = new NumericalFormat,
                                 ) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores, CONTENTION_LIMIT_WIDTH, missQueueDepth, repSetFormat) {

  override def printConfig(): Unit = println(s"Contention replacement policy configuration: " +
    s"base policy type: ${basePolicyType.getName}, " +
    s"replacement set format: ${repSetFormat.getName}, " +
    s"ways: $nWays, " +
    s"sets: $nSets, " +
    s"cores: $nCores, " +
    s"mim events: $enableMissInMiss, " +
    s"precedent events: $enablePrecedentEvents, " +
    s"wb events: $enableWbEvents." + "\n")

  override def includeCriticalMissQ(): Boolean = enableMissInMiss

  override def includeCriticalWbQ(): Boolean = enableWbEvents

  // ---------------- Read Stage ----------------
  val basePolRead = Module(basePolicyType.buildBasePolicyRead(nWays, nSets, repSetFormat))
  val assignArr = Module(new LineAssignmentsArray(nWays, nSets, nCores))

  val wbStageSetIdx = WireDefault(0.U(log2Up(nSets).W))
  val wbStageMruBits = WireDefault(0.U(basePolRead.stateWidth.W))
  val wbStageRepWay = WireDefault(0.U(log2Up(nWays).W))
  val wbStageUpdateCore = WireDefault(0.U(log2Up(nCores).W))
  val wbStageInsertBubble = WireDefault(false.B)

  // Need to delay this signal by two CCs since the bit plru uses memory to store the MRU bits
  val idxDelayReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall && !wbStageInsertBubble)
  val fwdMruBits = wbStageSetIdx === idxDelayReg && io.control.update

  basePolRead.io.stall := io.control.stall || wbStageInsertBubble
  basePolRead.io.rIdx := io.control.setIdx
  basePolRead.io.wrEn := io.control.update
  basePolRead.io.wIdx := wbStageSetIdx
  basePolRead.io.wData := wbStageMruBits
  basePolRead.io.fwd := fwdMruBits

  assignArr.io.stall := io.control.stall || wbStageInsertBubble
  assignArr.io.rSet := idxDelayReg
  assignArr.io.wrEn := io.control.evict
  assignArr.io.wrSet := wbStageSetIdx
  assignArr.io.wrWay := wbStageRepWay
  assignArr.io.wrLineAssign := wbStageUpdateCore

  val repSetPipeReg = PipelineReg(basePolRead.io.replacementSet, getDefaultRepSet, !io.control.stall)
  val reqValidPipeReg1 = PipelineReg(io.control.valid, false.B, !io.control.stall && !wbStageInsertBubble)
  val coreIdPipeReg1 = PipelineReg(io.control.coreId, 0.U, !io.control.stall && !wbStageInsertBubble)
  val mruBitsPipeReg1 = PipelineReg(basePolRead.io.readState, 0.U, !io.control.stall)
  val setIdxPipeReg1 = PipelineReg(idxDelayReg, 0.U, !io.control.stall && !wbStageInsertBubble)

  // ---------------- Update stage ----------------
  val coreTable = Module(new CoreContentionTable(nCores))
  val critFilter = Module(new CriticalityFilter(nWays, nCores, repSetFormat))
  val coreLimitUpdtCtrl = Module(new CoreLimitUpdateCtrl(nCores, nWays, missQueueDepth, enableMissInMiss, enableWbEvents, enablePrecedentEvents))
  val basePolUpdate = Module(basePolicyType.buildBasePolicyUpdate(nWays))

  critFilter.io.reqCore := coreIdPipeReg1
  critFilter.io.baseCandidates := repSetPipeReg
  critFilter.io.lineAssignments := assignArr.io.rLineAssign
  critFilter.io.validLineAssignments := assignArr.io.rValidAssign
  critFilter.io.coreLimits := coreTable.io.rLimits
  critFilter.io.criticalCores := coreTable.io.rCritCores

  coreTable.io.stall := io.control.stall
  coreTable.io.schedCoreId := io.scheduler.addr
  coreTable.io.setCritical := io.scheduler.cmd === SchedulerCmd.WR
  coreTable.io.unsetCritical := io.scheduler.cmd === SchedulerCmd.RD
  coreTable.io.setContLimit := io.scheduler.wData
  io.scheduler.rData := coreTable.io.readData
  coreTable.io.wrEns(0) := io.control.evict || io.control.update
  coreTable.io.wrEns(1) := coreLimitUpdtCtrl.io.updtCore2En
  coreTable.io.wrCoreIds(0) := coreLimitUpdtCtrl.io.updtCore1
  coreTable.io.wrCoreIds(1) := coreLimitUpdtCtrl.io.updtCore2
  coreTable.io.wrCoreLimits(0) := coreLimitUpdtCtrl.io.updtCore1Val
  coreTable.io.wrCoreLimits(1) := coreLimitUpdtCtrl.io.updtCore2Val

  // Update base policy
  basePolUpdate.io.hit := io.info.isHit
  basePolUpdate.io.hitWay := io.info.hitWay
  basePolUpdate.io.repWay := critFilter.io.replacementWay.bits
  basePolUpdate.io.stateIn := mruBitsPipeReg1

  val isRepWayCritPipeReg = PipelineReg(critFilter.io.isReplacementWayCrit, false.B, !io.control.stall, wbStageInsertBubble)
  val isRepWayAtLimitPipeReg = PipelineReg(critFilter.io.isReplacementWayAtLimit, false.B, !io.control.stall, wbStageInsertBubble)
  val repWayPipeReg = PipelineReg(critFilter.io.replacementWay.bits, 0.U, !io.control.stall, wbStageInsertBubble)
  val repWayValidPipeReg = PipelineReg(critFilter.io.replacementWay.valid, 0.U, !io.control.stall, wbStageInsertBubble)
  val isReqHitPipeReg = PipelineReg(io.info.isHit, 0.U, !io.control.stall, wbStageInsertBubble)
  val hitWayPipeReg = PipelineReg(io.info.hitWay, 0.U, !io.control.stall, wbStageInsertBubble)
  val critCoresPipeReg = PipelineReg(coreTable.io.rCritCores, VecInit(Seq.fill(nCores)(false.B)), !io.control.stall, wbStageInsertBubble)
  val coresLimitsPipeReg = PipelineReg(coreTable.io.rLimits, VecInit(Seq.fill(nCores)(0.U(CONTENTION_LIMIT_WIDTH.W))), !io.control.stall, wbStageInsertBubble)
  val lineAssignPipeReg = PipelineReg(assignArr.io.rLineAssign, VecInit(Seq.fill(nWays)(0.U(log2Up(nCores).W))), !io.control.stall, wbStageInsertBubble)
  val lineValidAssignPipeReg = PipelineReg(assignArr.io.rValidAssign, VecInit(Seq.fill(nWays)(false.B)), !io.control.stall, wbStageInsertBubble)
  val firstUcSetWayCorePipeReg = PipelineReg(critFilter.io.firstUcSetWayCore, 0.U, !io.control.stall, wbStageInsertBubble)
  val evictEventPipeReg = PipelineReg(critFilter.io.evictionEvent, false.B, !io.control.stall, wbStageInsertBubble)
  val reqValidPipeReg2 = PipelineReg(reqValidPipeReg1, false.B, !io.control.stall, wbStageInsertBubble)
  val coreIdPipeReg2 = PipelineReg(coreIdPipeReg1, 0.U, !io.control.stall, wbStageInsertBubble)
  val mruBitsPipeReg2 = PipelineReg(basePolUpdate.io.stateOut, 0.U, !io.control.stall, wbStageInsertBubble)
  val setIdxPipeReg2 = PipelineReg(setIdxPipeReg1, 0.U, !io.control.stall, wbStageInsertBubble)

  // ---------------- WB stage ----------------
  coreLimitUpdtCtrl.io.evict := io.control.evict
  coreLimitUpdtCtrl.io.evictionEvent := evictEventPipeReg
  coreLimitUpdtCtrl.io.reqCore := coreIdPipeReg2
  coreLimitUpdtCtrl.io.criticalCores := critCoresPipeReg
  coreLimitUpdtCtrl.io.hitWayIdx := hitWayPipeReg
  coreLimitUpdtCtrl.io.isHit := isReqHitPipeReg
  coreLimitUpdtCtrl.io.firstUcSetWayCore := firstUcSetWayCorePipeReg
  coreLimitUpdtCtrl.io.lineAssignments := lineAssignPipeReg
  coreLimitUpdtCtrl.io.validLineAssignments := lineValidAssignPipeReg
  coreLimitUpdtCtrl.io.coreLimits := coresLimitsPipeReg
  coreLimitUpdtCtrl.io.missQueueValidCores := io.info.missQueueValidReqs
  coreLimitUpdtCtrl.io.missQueueCritCores := io.info.missQueueCritReqs
  coreLimitUpdtCtrl.io.wbQueueValidCores := io.info.wbQueueValidReqs
  coreLimitUpdtCtrl.io.wbQueueCritCores := io.info.wbQueueCritReqs

  // Base policy forwarding signals
  wbStageSetIdx := setIdxPipeReg2
  wbStageRepWay := repWayPipeReg
  wbStageMruBits := mruBitsPipeReg2
  wbStageUpdateCore := coreIdPipeReg2
  wbStageInsertBubble := reqValidPipeReg2 && ((wbStageSetIdx === setIdxPipeReg1 && io.control.update) || io.control.evict)

  io.control.replaceWay := repWayPipeReg
  io.control.isValid := repWayValidPipeReg
  io.control.insertBubble := wbStageInsertBubble

  io.info.isReplacementWayCrit := isRepWayCritPipeReg
  io.info.isReplacementWayAtLimit := isRepWayAtLimitPipeReg
  io.info.updateCoreReachedLimit := critCoresPipeReg(coreIdPipeReg2) && (coresLimitsPipeReg(coreIdPipeReg2) === 0.U)
  io.info.updateCoreIsCrit := critCoresPipeReg(coreIdPipeReg2)
}