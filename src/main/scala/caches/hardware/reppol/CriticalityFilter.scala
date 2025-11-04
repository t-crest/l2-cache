package caches.hardware.reppol

import caches.hardware.util.Constants._
import chisel3._
import chisel3.util._

class CriticalityFilterIO(nWays: Int, nCores: Int, repSetFormat: BaseReplacementSetFormat) extends Bundle {
  val reqCore = Input(UInt(log2Up(nCores).W))
  val baseCandidates = repSetFormat match {
    case NumericalFormat() => Input(Vec(nWays, UInt(log2Up(nWays).W)))
    case MruFormat() => Input(Vec(nWays, UInt(1.W)))
    case _ => throw new IllegalArgumentException("Unrecognized replacement set format.")
  }
  val lineAssignments = Input(Vec(nWays, UInt(log2Up(nCores).W)))
  val validLineAssignments = Input(Vec(nWays, Bool()))
  val coreLimits = Input(Vec(nCores, UInt(CONTENTION_LIMIT_WIDTH.W)))
  val criticalCores = Input(Vec(nCores, Bool()))
  val replacementWay = Output(Valid(UInt(log2Up(nWays).W)))
  val firstUcSetWayCore = Output(UInt(log2Up(nCores).W))
  val evictionEvent = Output(Bool())
  val isReplacementWayCrit = Output(Bool())
  val isReplacementWayAtLimit = Output(Bool())
}

/**
 * Criticality filter returns the first unlimited way for eviction: the way that is not assigned to a critical core,
 * or is assigned to a critical core that has not yet reached its contention limit.
 */
class CriticalityFilter(nWays: Int, nCores: Int, repSetFormat: BaseReplacementSetFormat) extends Module {
  val io = IO(new CriticalityFilterIO(nWays, nCores, repSetFormat))

  def mruFilter(baseCandidates: Vec[UInt], ucMask: Vec[Bool]): UInt = {
    val anyUcInFirstSet = (~baseCandidates.asUInt).asUInt & ucMask.asUInt
    val anyUcInSecondSet = baseCandidates.asUInt & ucMask.asUInt
    val baseCandMask = WireDefault(0.U(nWays.W))

    when(anyUcInFirstSet.orR) {
      baseCandMask := anyUcInFirstSet
    }.otherwise {
      baseCandMask := anyUcInSecondSet
    }

    PriorityEncoder(baseCandMask)
  }

  def numericalFilter(baseCandidates: Vec[UInt], ucMask: Vec[Bool]): UInt = {
    // Order the UC mask
    val orderedUcMask = VecInit(Seq.fill(nWays)(false.B))
    for (i <- 0 until nWays) {
      orderedUcMask(i) := ucMask(baseCandidates(i))
    }

    baseCandidates(PriorityEncoder(orderedUcMask))
  }

  // Default signals
  val replaceWay = WireDefault(0.U(log2Up(nWays).W))
  val isValidReplaceWay = WireDefault(false.B)
  val isReplaceWayCrit = WireDefault(false.B)
  val isRepWayAtLimit = WireDefault(false.B)
  val criticalWays = VecInit(Seq.fill(nWays)(false.B))
  val unlimitedWays = VecInit(Seq.fill(nWays)(false.B))
  val coreAssignments = VecInit(Seq.fill(nWays)(0.U(log2Up(nCores).W)))

  // Compute UC mask
  for (wayIdx <- 0 until nWays) {
    val assignedCoreIdx = io.lineAssignments(wayIdx)
    val hasValidAssignment = io.validLineAssignments(wayIdx)

    val limit = io.coreLimits(assignedCoreIdx)
    val isCriticalWay = hasValidAssignment && io.criticalCores(assignedCoreIdx)
    val criticalNotAtLimit = isCriticalWay && limit > 0.U

    val isUnlimited = !isCriticalWay || criticalNotAtLimit

    unlimitedWays(wayIdx) := isUnlimited
    criticalWays(wayIdx) := isCriticalWay
    coreAssignments(wayIdx) := assignedCoreIdx
  }

  val firstUCWay = repSetFormat match {
    case NumericalFormat() => numericalFilter(io.baseCandidates, unlimitedWays)
    case MruFormat() => mruFilter(io.baseCandidates, unlimitedWays)
    case _ => throw new IllegalArgumentException("Unrecognized replacement set format.")
  }

  val firstUCSetWayIsCritical = criticalWays(firstUCWay)
  val isReqCoreCritical = io.criticalCores(io.reqCore)
  val firstUCSetWayCore = coreAssignments(firstUCWay)
  val anyNonCriticalWays = criticalWays.map(x => !x).reduce((x, y) => x || y)
  val evictionEvent = WireDefault(false.B)
  val replacementEvent = WireDefault(false.B)

  when(unlimitedWays.reduce((x, y) => x || y)) {
    // If we encounter a contention event we need to update the contention count
    evictionEvent := firstUCSetWayIsCritical && anyNonCriticalWays
    replacementEvent := firstUCSetWayIsCritical && !isReqCoreCritical
    replaceWay := firstUCWay
    isReplaceWayCrit := firstUCSetWayIsCritical
    isValidReplaceWay := true.B
    isRepWayAtLimit := false.B
  }.elsewhen(isReqCoreCritical) {
    replaceWay := io.baseCandidates(0) // Critical core, can evict any line
    isReplaceWayCrit := criticalWays(io.baseCandidates(0))
    isValidReplaceWay := true.B
    isRepWayAtLimit := true.B
  }

  io.firstUcSetWayCore := firstUCSetWayCore
  io.evictionEvent := evictionEvent || replacementEvent
  io.replacementWay.valid := isValidReplaceWay
  io.replacementWay.bits := replaceWay
  io.isReplacementWayCrit := isReplaceWayCrit // Need to know if the replacement way belongs to a critical core
  io.isReplacementWayAtLimit := isRepWayAtLimit // Need to know if the core to which the line belongs to reached limit
}
