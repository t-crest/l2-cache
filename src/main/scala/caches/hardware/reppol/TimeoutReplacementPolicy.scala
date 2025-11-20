package caches.hardware.reppol

import caches.hardware.util.Constants.TIMEOUT_LIMIT_WIDTH
import caches.hardware.util.{PipelineReg, TwoReadMemBlock}
import chisel3._
import chisel3.util._

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

  override def includeCriticalMissQ(): Boolean = true

  override def includeCriticalWbQ(): Boolean = false

//  //--------------- Read Stage ---------------------
//  val basePolRead = Module(basePolicyType.buildBasePolicyRead(nWays, nSets, repSetFormat))
//  val timerMemory = Module(new TimerMemory(nWays, nSets))
//
//  // Need to delay this signal by two CCs because PLRU has 2 stages
//  val idxDelayReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall)
//  val wbStageSetIdx = WireDefault(0.U(log2Up(nSets).W))
//  val wbStageMruBits = WireDefault(0.U(basePolRead.stateWidth.W))
//  val decrementIdx = WireDefault(0.U(log2Up(nSets).W))
//  val timerMemWrEn = WireDefault(false.B)
//  val timerMemWrData = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
//  val timerMemWrIdx = WireDefault(0.U(log2Up(nSets).W))
//
//  basePolRead.io.stall := io.control.stall
//  basePolRead.io.rIdx := io.control.setIdx
//  basePolRead.io.wrEn := io.control.update.valid
//  basePolRead.io.wIdx := wbStageSetIdx
//  basePolRead.io.wData := wbStageMruBits
//  basePolRead.io.fwd := wbStageSetIdx === idxDelayReg && io.control.update.valid
//
//  timerMemory.io.stall := io.control.stall
//  timerMemory.io.rIdx1 := decrementIdx
//  timerMemory.io.rIdx2 := idxDelayReg
//  timerMemory.io.wrEn := timerMemWrEn
//  timerMemory.io.wIdx := timerMemWrIdx
//  timerMemory.io.wData := timerMemWrData
//
//  val replaceSetPipeReg = PipelineReg(basePolRead.io.replacementSet, getDefaultRepSet, !io.control.stall)
//  val mruBitsPipeReg = PipelineReg(basePolRead.io.readState, 0.U, !io.control.stall)
//  val setIdxPipeReg = PipelineReg(idxDelayReg, 0.U, !io.control.stall)
//
//  // ---------------- Update stage ----------------
//  val coreTimeoutTable = Module(new CoreTimeoutTable(nCores))
//  val timeoutFilter = Module(new TimeoutFilter(nWays, nSets, nCores, repSetFormat))
//  val basePolUpdate = Module(basePolicyType.buildBasePolicyUpdate(nWays, repSetFormat))
//
//  coreTimeoutTable.io.scheduler <> io.scheduler
//
//  timeoutFilter.io.setIdx := setIdxPipeReg
//  timeoutFilter.io.update := io.control.update
////  timeoutFilter.io.updateCoreId := io.info.updateCoreId
//  timeoutFilter.io.baseCandidates := replaceSetPipeReg
//  timeoutFilter.io.coreTimeouts := coreTimeoutTable.io.rCoreTimeouts
//  timeoutFilter.io.decIdxTimers := timerMemory.io.rTimers1
//  timeoutFilter.io.updateIdxTimers := timerMemory.io.rTimers2
//
//  // Update base policy
//  basePolUpdate.io.hitWay := io.control.update.bits
//  basePolUpdate.io.stateIn := mruBitsPipeReg
//
//  // Base policy forwarding signals
//  wbStageSetIdx := setIdxPipeReg
//  wbStageMruBits := basePolUpdate.io.stateOut
//
//  // Timer memory write signals
//  decrementIdx := timeoutFilter.io.decIdx
//  timerMemWrEn := true.B // TODO: Should write enable always be high since we are always decrementing something ???
//  timerMemWrData := timeoutFilter.io.wTimers
//  timerMemWrIdx := timeoutFilter.io.wIdx
//
//  // Default output assignments
//  io.control <> 0.U.asTypeOf(io.control)
//  io.info <> 0.U.asTypeOf(io.info)
//
//  io.control.replaceWay := timeoutFilter.io.replaceWay
//  io.control.isValid := timeoutFilter.io.isRepValid

    // Default output assignments
    io.control <> 0.U.asTypeOf(io.control)
    io.info <> 0.U.asTypeOf(io.info)
    io.scheduler <> 0.U.asTypeOf(io.scheduler)
}
