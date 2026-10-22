package caches.hardware.reppol

import caches.hardware.util.PipelineReg
import chisel3._
import chisel3.util._

/**
 * Critical way partitioning replacement policy. The ways of each set are split into a critical partition, ways
 * 0 to k-1, and a non-critical partition, ways k to nWays-1. A critical core only replaces lines in the critical
 * partition and a non-critical core only in the non-critical partition. Hits are served from any way. Within a
 * partition, the least recently used way is selected with bit PLRU. Requests are never rejected.
 *
 * Without priorityQueues, no priority queues are used. With priorityQueues set, the cache includes the priority miss
 * and write-back queues of the contention policy, and a critical core is treated like a contention policy core that
 * has reached its limit: all its misses are pushed to the priority miss queue, and the write-backs of lines in the
 * critical partition to the priority write-back queue.
 *
 * The bit PLRU state is updated per partition: when all ways of the accessed partition are recently used, only the
 * bits of that partition are cleared, so accesses to one partition do not change the order of the other.
 *
 * The scheduler interface is compatible with the contention policy: a write sets the core as critical and, if the
 * written value is not zero, sets the number of critical ways k (clamped to 1 to nWays-1). A read unsets the core,
 * returning 1 if the core was critical. Reset unsets all cores and restores the default number of critical ways.
 *
 * Pipeline (same division of work as the contention policy, each stage is one cycle):
 *  - Read stage:   the bit PLRU state of the requested set is read from memory and the ordered replacement candidates
 *                  are computed from it, with the state written by the request in the WB stage forwarded.
 *  - Update stage: the registered candidates are filtered by partition; select the way to replace and compute the new
 *                  state.
 *  - WB stage:     io.control.update tells whether the request hit or allocated a line; the new state is written back.
 *
 * The state of the request in the update stage is not forwarded. If the request in the WB stage updates the same set,
 * a bubble is inserted instead: the request stays in the update stage for one more cycle, with its state and
 * candidates taken from the state written by the WB stage.
 *
 * @param nWays                number of ways in a single cache set
 * @param nSets                number of sets in the whole cache
 * @param nCores               number of cores sharing the cache
 * @param defaultCriticalWays  number of critical ways after reset
 * @param schedulerDataWidth   width of the data written and read through the scheduler interface
 * @param missQueueDepth       number of entries in the miss queue
 * @param priorityQueues       use the priority miss and write-back queues for critical misses and write-backs
 */
class CriticalWayPartitionReplacementPolicy(
                                             nWays: Int,
                                             nSets: Int,
                                             nCores: Int,
                                             defaultCriticalWays: Int = -1,
                                             schedulerDataWidth: Int = 32,
                                             missQueueDepth: Int = 4,
                                             priorityQueues: Boolean = false
                                           ) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores, schedulerDataWidth, missQueueDepth, new NumericalFormat) {
  require(nWays >= 2, "Way partitioning needs at least two ways")

  private val resetCriticalWays = if (defaultCriticalWays == -1) nWays / 2 else defaultCriticalWays
  require(resetCriticalWays >= 1 && resetCriticalWays < nWays, "The default number of critical ways must be between 1 and nWays-1")

  override def printConfig(): Unit = println(s"Critical way partition replacement policy configuration: " +
    s"default critical ways: $resetCriticalWays, " +
    s"priority queues: $priorityQueues, " +
    s"ways: $nWays, " +
    s"sets: $nSets, " +
    s"cores: $nCores.")

  override def includeCriticalMissQ(): Boolean = priorityQueues

  override def includeCriticalWbQ(): Boolean = priorityQueues

  // ---------------- Critical cores and partition ----------------
  val criticalCores = RegInit(VecInit(Seq.fill(nCores)(false.B)))
  val criticalWays = RegInit(resetCriticalWays.U(log2Up(nWays).W))
  // Bit i is set if way i belongs to the critical partition
  val criticalWayMask = ((1.U((nWays + 1).W) << criticalWays).asUInt - 1.U)(nWays - 1, 0)

  io.scheduler.rData := 0.U
  switch(io.scheduler.cmd) {
    is(SchedulerCmd.WR) {
      criticalCores(io.scheduler.addr) := true.B
      when(io.scheduler.wData =/= 0.U) {
        criticalWays := Mux(io.scheduler.wData >= nWays.U, (nWays - 1).U, io.scheduler.wData)
      }
    }
    is(SchedulerCmd.RD) {
      criticalCores(io.scheduler.addr) := false.B
      io.scheduler.rData := criticalCores(io.scheduler.addr)
    }
    is(SchedulerCmd.RST) {
      criticalCores.foreach(_ := false.B)
      criticalWays := resetCriticalWays.U
    }
  }

  /** Marks the accessed way as recently used, and clears the other bits of its partition once all are set. */
  def updateState(way: UInt, state: UInt): UInt = {
    val wayBit = UIntToOH(way, nWays)
    val partition = Mux((criticalWayMask & wayBit).orR, criticalWayMask, (~criticalWayMask).asUInt(nWays - 1, 0))
    val newState = state | wayBit
    Mux((newState & partition) === partition, (state & (~partition).asUInt) | wayBit, newState)
  }

  // ---------------- Read stage ----------------
  val basePolRead = Module(new BitPlruReadStage(nWays, nSets, new NumericalFormat))

  val insertBubble = WireDefault(false.B)
  val wbStageIdx = WireDefault(0.U(log2Up(nSets).W))
  val wbStageState = WireDefault(0.U(nWays.W))

  // Delayed by one cycle since the bit PLRU state is stored in memory. Held in a bubble, like the cache holds the
  // younger requests.
  val idxDelayReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall && !insertBubble)
  val fwd1 = wbStageIdx === idxDelayReg && io.control.update

  basePolRead.io.stall := io.control.stall || insertBubble
  basePolRead.io.rIdx := io.control.setIdx
  basePolRead.io.wrEn := io.control.update
  basePolRead.io.wIdx := wbStageIdx
  basePolRead.io.wData := wbStageState
  basePolRead.io.fwd := fwd1

  // In a bubble, the request in the update stage stays and takes the state written by the WB stage, which updates the
  // same set. The memory reads the state of the next request again in the bubble, so the read stage output is dropped.
  // The way indices are cut to the width of the read stage's candidates
  val wbStageCandidates = VecInit(basePolRead.getNumericalReplacementSet(wbStageState).map(_(log2Up(nWays) - 1, 0)))
  val statePipeReg1 = PipelineReg(Mux(insertBubble, wbStageState, basePolRead.io.readState), 0.U, !io.control.stall)
  val repSetPipeReg1 = PipelineReg(Mux(insertBubble, wbStageCandidates, basePolRead.io.replacementSet), getDefaultRepSet, !io.control.stall)
  val setIdxPipeReg1 = PipelineReg(idxDelayReg, 0.U, !io.control.stall && !insertBubble)
  val coreIdPipeReg1 = PipelineReg(io.control.coreId, 0.U, !io.control.stall && !insertBubble)
  val reqValidPipeReg1 = PipelineReg(io.control.valid, false.B, !io.control.stall && !insertBubble)

  // ---------------- Update stage ----------------
  val baseCandidates = repSetPipeReg1

  // Partitioning: evict the first way of the requesting core's partition in bit PLRU order. Both partitions hold at
  // least one way, so there is always a candidate.
  val isReqCoreCrit = criticalCores(coreIdPipeReg1)
  val allowedWays = Mux(isReqCoreCrit, criticalWayMask, (~criticalWayMask).asUInt(nWays - 1, 0))
  val orderedAllowed = VecInit(baseCandidates.map(way => allowedWays(way)))
  val repWay = baseCandidates(PriorityEncoder(orderedAllowed))

  val newState = updateState(Mux(io.info.isHit, io.info.hitWay, repWay), statePipeReg1)

  // The WB stage is empty after a bubble
  val repWayPipeReg2 = PipelineReg(repWay, 0.U, !io.control.stall, insertBubble)
  val repSetPipeReg2 = PipelineReg(baseCandidates, getDefaultRepSet, !io.control.stall, insertBubble)
  val statePipeReg2 = PipelineReg(newState, 0.U, !io.control.stall, insertBubble)
  val setIdxPipeReg2 = PipelineReg(setIdxPipeReg1, 0.U, !io.control.stall, insertBubble)
  val coreIdPipeReg2 = PipelineReg(coreIdPipeReg1, 0.U, !io.control.stall, insertBubble)
  // Only critical cores replace lines in the critical partition, so these lines are critical
  val isRepWayCritPipeReg2 = PipelineReg(criticalWayMask(repWay), false.B, !io.control.stall, insertBubble)

  // ---------------- WB stage ----------------
  wbStageIdx := setIdxPipeReg2
  wbStageState := statePipeReg2

  // The state of the request in the update stage is stale if the WB stage updates the same set
  insertBubble := reqValidPipeReg1 && io.control.update && wbStageIdx === setIdxPipeReg1

  // Default output assignments
  io.control <> 0.U.asTypeOf(io.control)
  io.info <> 0.U.asTypeOf(io.info)

  io.control.replaceWay := repWayPipeReg2
  io.control.isValid := true.B // A request is never rejected
  io.control.insertBubble := insertBubble

  // With priorityQueues, a critical core is always at its limit, so its misses and the write-backs of lines in the
  // critical partition are pushed to the priority queues
  val isUpdateCoreCrit = criticalCores(coreIdPipeReg2)
  io.info.updateCoreIsCrit := isUpdateCoreCrit
  io.info.updateCoreReachedLimit := priorityQueues.B && isUpdateCoreCrit
  io.info.isReplacementWayCrit := isRepWayCritPipeReg2
  io.info.isReplacementWayAtLimit := priorityQueues.B

  // Debugging signal
  repSet := repSetPipeReg2
}
