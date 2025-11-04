package caches.hardware.reppol

import caches.hardware.util.PipelineReg
import chisel3._
import chisel3.util._

/**
 * Binary tree based implementation of Pseudo LRU
 *
 * @param nWays number of ways in a single cache set
 * @param nSets number of sets in the whole cache
 * @param nCores number of cores sharing the cache
 */
class TreePlruReplacementPolicy(nWays: Int, nSets: Int, nCores: Int, repSetFormat: BaseReplacementSetFormat = new NumericalFormat) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores, repSetFormat = repSetFormat) {
  override def printConfig(): Unit = println(s"Tree PLRU replacement policy configuration: " +
    s"ways: $nWays, " +
    s"sets: $nSets, " +
    s"cores: $nCores" + "\n")

  override def includeCriticalMissQ(): Boolean = false

  override def includeCriticalWbQ(): Boolean = false

  // ---------------- Read stage ----------------
  val idxDelayReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall)

  val wbStageIdx = WireDefault(0.U(log2Up(nSets).W))
  val wbStageRepWay = WireDefault(0.U(log2Up(nWays).W))
  val wbStageMruBits = WireDefault(0.U(nWays.W))

  val bitPlruRead = Module(new TreePlruReadStage(nWays, nSets, repSetFormat))

  val fwd1 = wbStageIdx === idxDelayReg && io.control.update

  bitPlruRead.io.stall := io.control.stall
  bitPlruRead.io.rIdx := io.control.setIdx
  bitPlruRead.io.wrEn := io.control.update
  bitPlruRead.io.wIdx := wbStageIdx
  bitPlruRead.io.wData := wbStageMruBits
  bitPlruRead.io.fwd := fwd1

  val repWayPipeReg1 = PipelineReg(bitPlruRead.io.replaceWay, 0.U, !io.control.stall)
  val repSetPipeReg1 = PipelineReg(bitPlruRead.io.replacementSet, getDefaultRepSet, !io.control.stall)
  val mruBitsPipeReg1 = PipelineReg(bitPlruRead.io.readState, 0.U, !io.control.stall)
  val setIdxPipeReg1 = PipelineReg(idxDelayReg, 0.U, !io.control.stall)

  // ---------------- Update stage ----------------
  val bitPlruUpdate = Module(new TreePlruUpdateStage(nWays))

  val fwd2 = wbStageIdx === setIdxPipeReg1 && io.control.update
  val inRepWay = Mux(fwd2, wbStageRepWay, repWayPipeReg1)
  val inMruBits = Mux(fwd2, wbStageMruBits, mruBitsPipeReg1)

  // Update base policy
  bitPlruUpdate.io.hit := io.info.isHit
  bitPlruUpdate.io.hitWay := io.info.hitWay
  bitPlruUpdate.io.repWay := inRepWay
  bitPlruUpdate.io.stateIn := inMruBits

  val newRepWayPipeReg = PipelineReg(bitPlruUpdate.io.repWayOut, 0.U, !io.control.stall)
  val repWayPipeReg2 = PipelineReg(inRepWay, 0.U, !io.control.stall)
  val repSetPipeReg2 = PipelineReg(repSetPipeReg1, getDefaultRepSet, !io.control.stall)
  val mruBitsPipeReg2 = PipelineReg(bitPlruUpdate.io.stateOut, 0.U, !io.control.stall)
  val setIdxPipeReg2 = PipelineReg(setIdxPipeReg1, 0.U, !io.control.stall)

  // ---------------- WB stage ----------------
  // Forwarding signals
  wbStageIdx := setIdxPipeReg2
  wbStageMruBits := mruBitsPipeReg2
  wbStageRepWay := newRepWayPipeReg

  // Default output assignments
  io.control <> 0.U.asTypeOf(io.control)
  io.info <> 0.U.asTypeOf(io.info)
  io.scheduler <> 0.U.asTypeOf(io.scheduler)

  // We assign only the signals that are relevant to this policy
  io.control.replaceWay := repWayPipeReg2
  io.control.isValid := true.B

  // Debugging signal
  repSet := repSetPipeReg2
}

