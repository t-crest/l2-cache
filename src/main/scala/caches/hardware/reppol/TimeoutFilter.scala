package caches.hardware.reppol

import caches.hardware.util.Constants.TIMEOUT_LIMIT_WIDTH
import chisel3._
import chisel3.util._

class TimeoutFilterIO(nWays: Int, nSets: Int, nCores: Int, repSetFormat: BaseReplacementSetFormat) extends Bundle {
  val setIdx = Input(UInt(log2Up(nSets).W))
  val decIdx = Input(UInt(log2Up(nSets).W))
  val isHit = Input(Bool())
  val hitWayIdx = Input(UInt(log2Up(nWays).W))
  val reqCoreId = Input(UInt(log2Up(nCores).W))
  val baseCandidates = repSetFormat match {
    case NumericalFormat() => Input(Vec(nWays, UInt(log2Up(nWays).W)))
    case MruFormat() => Input(Vec(nWays, UInt(1.W)))
    case _ => throw new IllegalArgumentException("Unrecognized replacement set format.")
  }
  val coreTimeouts = Input(Vec(nCores, UInt(TIMEOUT_LIMIT_WIDTH.W)))
  val decIdxTimers = Input(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
  val setIdxTimers = Input(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
  val replacementWay = Output(Valid(UInt(log2Up(nWays).W)))
  val wDecTimers = Output(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
  val wRefreshTimers = Output(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
}

class TimeoutFilter(nWays: Int, nSets: Int, nCores: Int, repSetFormat: BaseReplacementSetFormat) extends Module {
  val io = IO(new TimeoutFilterIO(nWays, nSets, nCores, repSetFormat))

  def mruFilter(baseCandidates: Vec[UInt], timeoutMask: Vec[Bool]): UInt = {
    val anyTimeoutInFirstSet = (~baseCandidates.asUInt).asUInt & timeoutMask.asUInt
    val anyTimeoutInSecondSet = baseCandidates.asUInt & timeoutMask.asUInt
    val baseCandMask = WireDefault(0.U(nWays.W))

    when(anyTimeoutInFirstSet.asUInt.orR) {
      baseCandMask := anyTimeoutInFirstSet
    } .otherwise {
      baseCandMask := anyTimeoutInSecondSet
    }

    PriorityEncoder(baseCandMask)
  }

  def numericalFilter(baseCandidates: Vec[UInt], timeoutMask: Vec[Bool]): UInt = {
    val baseCandMask = VecInit(Seq.fill(nWays)(true.B))
    for (i <- 0 until nWays) {
      val wayIdx = baseCandidates(i)
      val timedOut = timeoutMask(wayIdx)

      baseCandMask(i) := timedOut
    }

    baseCandidates(PriorityEncoder(baseCandMask))
  }

  def isCritical(coreIdx: UInt, coreTmts: Vec[UInt]): Bool = coreTmts(coreIdx) =/= 0.U

  val isRepValid = WireDefault(false.B)
  val replaceWay = WireDefault(0.U(log2Up(nWays).W))
  val isReqCoreCrit = isCritical(io.reqCoreId, io.coreTimeouts)

  val timedOutWays = VecInit(Seq.fill(nWays)(true.B))
  for (wayIdx <- 0 until nWays) {
    timedOutWays(wayIdx) := io.setIdxTimers(wayIdx) === 0.U
  }

  val firstTimedOutWay = repSetFormat match {
    case NumericalFormat() => numericalFilter(io.baseCandidates, timedOutWays)
    case MruFormat() => mruFilter(io.baseCandidates, timedOutWays)
    case _ => throw new IllegalArgumentException("Unrecognized replacement set format.")
  }

  val anyTimedOutWays = timedOutWays.reduce((x, y) => x || y)

  isRepValid := anyTimedOutWays || isReqCoreCrit
  when(!anyTimedOutWays && isReqCoreCrit) {
    replaceWay := io.baseCandidates(0)
  }.otherwise {
    replaceWay := firstTimedOutWay
  }

  //------------- Timers update -----------------
  val refreshWayIdx = Mux(io.isHit, io.hitWayIdx, replaceWay)
  val refreshVal = io.coreTimeouts(io.reqCoreId)
  val wDecTimers = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
  val wRefreshTimers = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))

  // We decrement the current decrementation set's timers
  for (i <- 0 until nWays) {
    when(io.decIdxTimers(i) =/= 0.U) {
      wDecTimers(i) := io.decIdxTimers(i) - 1.U
    }
  }

  when (io.decIdx === io.setIdx) { // When the refresh set and dec set are the same
    val newRefreshTimers = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
    for (way <- 0 until nWays) {
      when(way.U === refreshWayIdx && (refreshVal > wDecTimers(way))) {
        newRefreshTimers(way) := refreshVal
      }.otherwise {
        newRefreshTimers(way) := wDecTimers(way)
      }
    }

    wRefreshTimers := newRefreshTimers
  } .otherwise{
    val newRefreshTimers = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
    for (way <- 0 until nWays) {
      when(way.U === refreshWayIdx && (refreshVal > io.setIdxTimers(way))) {
        newRefreshTimers(way) := refreshVal
      }.otherwise {
        newRefreshTimers(way) := io.setIdxTimers(way)
      }
    }

    wRefreshTimers := newRefreshTimers
  }

  io.replacementWay.valid := isRepValid
  io.replacementWay.bits := replaceWay
  io.wDecTimers := wDecTimers
  io.wRefreshTimers := wRefreshTimers
}
