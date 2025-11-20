package caches.hardware.reppol

import caches.hardware.util.Constants.TIMEOUT_LIMIT_WIDTH
import caches.hardware.util.{PipelineReg, TwoReadMemBlock}
import chisel3._
import chisel3.util._

class TimerUpdateCtrl(nWays: Int, nSets: Int) extends Module {
  val io = IO (new Bundle {
    val bubble = Input(Bool())
    val refresh = Input(Bool())
    val reqCoreSetIdx = Input(UInt(log2Up(nSets).W))
    val oldDecIdx = Input(UInt(log2Up(nSets).W))
    val decTimers = Input(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
    val refreshTimers = Input(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
    val wTimers = Output(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
    val rIdx = Output(UInt(log2Up(nSets).W))
    val wIdx = Output(UInt(log2Up(nSets).W))
    val wEn = Output(Bool())
  })

  val wTimers = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
  val wIdx = WireDefault(0.U(log2Up(nSets).W))
  val rIdx = WireDefault(0.U(log2Up(nSets).W))
  val wEn = WireDefault(false.B)

  val decIdx = RegInit(0.U(log2Up(nSets).W))

  when(io.refresh) {
    wTimers := io.refreshTimers
    wIdx := io.reqCoreSetIdx

    when(io.oldDecIdx === io.reqCoreSetIdx) {
      decIdx := decIdx + 1.U
      rIdx := decIdx
    }.otherwise {
      rIdx := io.oldDecIdx
    }
  } .otherwise {
    wTimers := io.decTimers
    decIdx := decIdx + 1.U
    wIdx := io.oldDecIdx
    rIdx := decIdx
  }

  when(!io.bubble) {
    wEn := true.B
  }

  io.rIdx := rIdx
  io.wIdx := wIdx
  io.wEn := wEn
  io.wTimers := wTimers
}

class CoreTimeoutTable(nCores: Int) extends Module {
  val io = IO(new Bundle {
    val scheduler = new SchedulerControlIO(nCores, TIMEOUT_LIMIT_WIDTH)
    val rCoreTimeouts = Output(Vec(nCores, UInt(TIMEOUT_LIMIT_WIDTH.W)))
  })

  val coreTimeouts = RegInit(VecInit(Seq.fill(nCores)(0.U(TIMEOUT_LIMIT_WIDTH.W))))
  val schedulerRData = WireDefault(0.U(TIMEOUT_LIMIT_WIDTH.W))

  // Connection to scheduler
  when(io.scheduler.cmd === SchedulerCmd.WR) {
    coreTimeouts(io.scheduler.addr) := io.scheduler.wData
  }.elsewhen(io.scheduler.cmd === SchedulerCmd.RD) {
    schedulerRData := coreTimeouts(io.scheduler.addr)
    coreTimeouts(io.scheduler.addr) := 0.U
  }

  io.scheduler.rData := schedulerRData
  io.rCoreTimeouts := coreTimeouts
}

class TimerMemory(nWays: Int, nSets: Int) extends Module {
  val io = IO(new Bundle{
    val stall = Input(Bool())
    val rIdx1 = Input(UInt(log2Up(nSets).W))
    val rIdx2 = Input(UInt(log2Up(nSets).W))
    val wrEn = Input(Bool())
    val wIdx = Input(UInt(log2Up(nSets).W))
    val wData = Input(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
//    val wMask = Input(Vec(nWays, Bool()))
    val rTimers1 = Output(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
    val rTimers2 = Output(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
  })

  val timers = Array.fill(nWays)(Module(new TwoReadMemBlock(nSets, TIMEOUT_LIMIT_WIDTH, stallReg1 = false)))

  val rTimers1 = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
  val rTimers2 = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
  for (wayIdx <- 0 until nWays) {
//    val wrWayEn = io.wMask(wayIdx)

    timers(wayIdx).io.wrEn := io.wrEn //&& wrWayEn
    timers(wayIdx).io.readAddr1 := io.rIdx1
    timers(wayIdx).io.readAddr2 := io.rIdx2
    timers(wayIdx).io.writeAddr := io.wIdx
    timers(wayIdx).io.writeData := io.wData(wayIdx)
    timers(wayIdx).io.stall := io.stall

    rTimers1(wayIdx) := timers(wayIdx).io.readData1
    rTimers2(wayIdx) := timers(wayIdx).io.readData2
  }

  io.rTimers1 := rTimers1
  io.rTimers2 := rTimers2
}

/**
 * Timeout replacement policy. When critical cores access a line, a counter is initiated for that line.
 * Any non-critical core can only evict lines whose timer has reached zero, i.e. lines that are owned by non-critical
 * cores or critical core lines that have timed out.
 */
class TimeoutReplacementPolicy(
                                nWays: Int,
                                nSets: Int,
                                nCores: Int,
                                basePolicyType: BasePolicyType,
                                repSetFormat: BaseReplacementSetFormat = new NumericalFormat
                              ) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores, TIMEOUT_LIMIT_WIDTH, repSetFormat = repSetFormat) {
  override def printConfig(): Unit = println(s"Timeout replacement policy configuration: " +
    s"base policy type: ${basePolicyType.getName}, " +
    s"replacement set format: ${repSetFormat.getName}, " +
    s"ways: $nWays, " +
    s"sets: $nSets, " +
    s"cores: $nCores"
  )

  override def includeCriticalMissQ(): Boolean = false

  override def includeCriticalWbQ(): Boolean = false

  // ---------------- Read Stage ----------------
  val basePolRead = Module(basePolicyType.buildBasePolicyRead(nWays, nSets, repSetFormat))
  val timerMemory = Module(new TimerMemory(nWays, nSets))

  val wbStageSetIdx = WireDefault(0.U(log2Up(nSets).W))
  val wbStageMruBits = WireDefault(0.U(basePolRead.stateWidth.W))
  val wbStageRepWay = WireDefault(0.U(log2Up(nWays).W))
  val wbStageUpdateCore = WireDefault(0.U(log2Up(nCores).W))
  val wbStageDecIdx = WireDefault(0.U(log2Up(nSets).W))
  val wbStageTimerMemWrEn = WireDefault(false.B)
  val wbStageTimerMemWrData = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
  val wbStageTimerMemWrIdx = WireDefault(0.U(log2Up(nSets).W))
  val wbStageInsertBubble = WireDefault(false.B)

  // Need to delay this signal by two CCs because PLRU has 2 stages
  val idxDelayReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall && !wbStageInsertBubble)
  val fwdMruBits = wbStageSetIdx === idxDelayReg && io.control.update

  basePolRead.io.stall := io.control.stall || wbStageInsertBubble
  basePolRead.io.rIdx := io.control.setIdx
  basePolRead.io.wrEn := io.control.update
  basePolRead.io.wIdx := wbStageSetIdx
  basePolRead.io.wData := wbStageMruBits
  basePolRead.io.fwd := fwdMruBits

  timerMemory.io.stall := io.control.stall || wbStageInsertBubble
  timerMemory.io.rIdx1 := wbStageDecIdx
  timerMemory.io.rIdx2 := idxDelayReg
  timerMemory.io.wrEn := wbStageTimerMemWrEn
  timerMemory.io.wIdx := wbStageTimerMemWrIdx
  timerMemory.io.wData := wbStageTimerMemWrData

  val replaceSetPipeReg = PipelineReg(basePolRead.io.replacementSet, getDefaultRepSet, !io.control.stall)
  val decIdxPipeReg1 = PipelineReg(wbStageDecIdx, 0.U, !io.control.stall)
  val reqValidPipeReg1 = PipelineReg(io.control.valid, false.B, !io.control.stall && !wbStageInsertBubble)
  val coreIdPipeReg1 = PipelineReg(io.control.coreId, 0.U, !io.control.stall && !wbStageInsertBubble)
  val mruBitsPipeReg1 = PipelineReg(basePolRead.io.readState, 0.U, !io.control.stall && !wbStageInsertBubble)
  val setIdxPipeReg1 = PipelineReg(idxDelayReg, 0.U, !io.control.stall && !wbStageInsertBubble)

  // ---------------- Update stage ----------------
  val coreTimeoutTable = Module(new CoreTimeoutTable(nCores))
  val coreTimerUpdtCtrl = Module(new TimerUpdateCtrl(nWays, nSets))
  val timeoutFilter = Module(new TimeoutFilter(nWays, nSets, nCores, repSetFormat))
  val basePolUpdate = Module(basePolicyType.buildBasePolicyUpdate(nWays))

  timeoutFilter.io.setIdx := setIdxPipeReg1
  timeoutFilter.io.decIdx := decIdxPipeReg1
  timeoutFilter.io.isHit := io.info.isHit
  timeoutFilter.io.hitWayIdx := io.info.hitWay
  timeoutFilter.io.reqCoreId := coreIdPipeReg1
  timeoutFilter.io.baseCandidates := replaceSetPipeReg
  timeoutFilter.io.coreTimeouts := coreTimeoutTable.io.rCoreTimeouts
  timeoutFilter.io.decIdxTimers := timerMemory.io.rTimers1
  timeoutFilter.io.setIdxTimers := timerMemory.io.rTimers2

  coreTimeoutTable.io.scheduler <> io.scheduler

  // Update base policy
  basePolUpdate.io.hit := io.info.isHit
  basePolUpdate.io.hitWay := io.info.hitWay
  basePolUpdate.io.repWay := timeoutFilter.io.replacementWay.bits
  basePolUpdate.io.stateIn := mruBitsPipeReg1

  val isBubblePipeReg = PipelineReg(false.B, true.B, !io.control.stall, wbStageInsertBubble)
  val wDecTimerPipeReg = PipelineReg(timeoutFilter.io.wDecTimers, VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W))), !io.control.stall, wbStageInsertBubble)
  val wRefreshTimerPipeReg = PipelineReg(timeoutFilter.io.wRefreshTimers, VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W))), !io.control.stall, wbStageInsertBubble)
  val repWayPipeReg = PipelineReg(timeoutFilter.io.replacementWay.bits, 0.U, !io.control.stall, wbStageInsertBubble)
  val repWayValidPipeReg = PipelineReg(timeoutFilter.io.replacementWay.valid, 0.U, !io.control.stall, wbStageInsertBubble)
  val isReqHitPipeReg = PipelineReg(io.info.isHit, 0.U, !io.control.stall, wbStageInsertBubble)
  val hitWayPipeReg = PipelineReg(io.info.hitWay, 0.U, !io.control.stall, wbStageInsertBubble)
  val decIdxPipeReg2 = PipelineReg(decIdxPipeReg1, 0.U, !io.control.stall, wbStageInsertBubble)
  val reqValidPipeReg2 = PipelineReg(reqValidPipeReg1, false.B, !io.control.stall, wbStageInsertBubble)
  val coreIdPipeReg2 = PipelineReg(coreIdPipeReg1, 0.U, !io.control.stall, wbStageInsertBubble)
  val mruBitsPipeReg2 = PipelineReg(basePolUpdate.io.stateOut, 0.U, !io.control.stall, wbStageInsertBubble)
  val setIdxPipeReg2 = PipelineReg(setIdxPipeReg1, 0.U, !io.control.stall, wbStageInsertBubble)

  // ---------------- WB stage ----------------
  coreTimerUpdtCtrl.io.bubble := isBubblePipeReg
  coreTimerUpdtCtrl.io.refresh := io.control.update
  coreTimerUpdtCtrl.io.reqCoreSetIdx := setIdxPipeReg2
  coreTimerUpdtCtrl.io.oldDecIdx := decIdxPipeReg2
  coreTimerUpdtCtrl.io.decTimers := wDecTimerPipeReg
  coreTimerUpdtCtrl.io.refreshTimers := wRefreshTimerPipeReg

  // Base policy forwarding signals
  wbStageSetIdx := setIdxPipeReg2
  wbStageRepWay := repWayPipeReg
  wbStageMruBits := mruBitsPipeReg2
  wbStageUpdateCore := coreIdPipeReg2
  wbStageDecIdx := coreTimerUpdtCtrl.io.rIdx
  wbStageTimerMemWrEn := coreTimerUpdtCtrl.io.wEn
  wbStageTimerMemWrData := coreTimerUpdtCtrl.io.wTimers
  wbStageTimerMemWrIdx := coreTimerUpdtCtrl.io.wIdx
  wbStageInsertBubble := reqValidPipeReg2 && ((((wbStageSetIdx === setIdxPipeReg1) || (wbStageSetIdx === decIdxPipeReg1)) && io.control.update) || io.control.evict)

  // Default output assignments
  io.control <> 0.U.asTypeOf(io.control)
  io.info <> 0.U.asTypeOf(io.info)

  io.control.replaceWay := repWayPipeReg
  io.control.isValid := repWayValidPipeReg
  io.control.insertBubble := wbStageInsertBubble
}
