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
 * core. Without priorityQueues, unlike the contention policy, no priority queues are used.
 *
 * With rejectNonCritical set, a miss of a non-critical core to a set in which all lines are critical is rejected
 * instead, and retried later through the rejection queue. A critical core still evicts the first line.
 *
 * With priorityQueues set, the cache includes the priority miss and write-back queues of the contention policy, and a
 * critical core is treated like a contention policy core that has reached its limit: all its misses are pushed to the
 * priority miss queue, and the write-backs of critical lines to the priority write-back queue.
 *
 * The scheduler interface is compatible with the contention policy: a write sets the core as critical (the written
 * value is ignored) and a read unsets it, returning 1 if the core was critical. Reset unsets all cores.
 *
 * Pipeline (same division of work as the contention policy, each stage is one cycle):
 *  - Read stage:   the base policy state of the requested set is read from memory and the ordered replacement
 *                  candidates are computed from it, with the state written by the request in the WB stage forwarded.
 *                  The line assignments are read from memory and arrive in the update stage.
 *  - Update stage: the registered candidates are filtered by criticality; select the way to replace and compute the
 *                  new state.
 *  - WB stage:     io.control.update/evict tell whether the request hit (update) or allocated a line (evict);
 *                  the new state and line assignment are written back.
 *
 * The state and the line assignments of the request in the update stage are not forwarded. If the request in the WB
 * stage updates the same set, a bubble is inserted instead: the request stays in the update stage for one more cycle,
 * with its state and candidates taken from the state written by the WB stage, and its line assignments read again.
 *
 * @param nWays              number of ways in a single cache set
 * @param nSets              number of sets in the whole cache
 * @param nCores             number of cores sharing the cache
 * @param basePolicyType     base policy that orders the replacement candidates
 * @param claimOnHit         assign a non-critical line to a critical core when the critical core hits it
 * @param rejectNonCritical  reject a non-critical miss when all lines in the set are critical
 * @param priorityQueues     use the priority miss and write-back queues for critical misses and write-backs
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
                               priorityQueues: Boolean = false,
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
    s"priority queues: $priorityQueues, " +
    s"ways: $nWays, " +
    s"sets: $nSets, " +
    s"cores: $nCores.")

  // LC: without priorityQueues replacement only, so no priority miss and write-back queues
  override def includeCriticalMissQ(): Boolean = priorityQueues

  override def includeCriticalWbQ(): Boolean = priorityQueues

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
  val insertBubble = WireDefault(false.B)
  val wbStageIdx = WireDefault(0.U(log2Up(nSets).W))
  val wbStageState = WireDefault(0.U(basePolRead.stateWidth.W))
  val wbStageAssignWrEn = WireDefault(false.B)
  val wbStageAssignWay = WireDefault(0.U(log2Up(nWays).W))
  val wbStageAssignCore = WireDefault(0.U(log2Up(nCores).W))

  // Delayed by one cycle since the base policy state is stored in memory. Held in a bubble, like the cache holds the
  // younger requests.
  val idxDelayReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall && !insertBubble)
  val fwd1 = wbStageIdx === idxDelayReg && io.control.update

  basePolRead.io.stall := io.control.stall || insertBubble
  basePolRead.io.rIdx := io.control.setIdx
  basePolRead.io.wrEn := io.control.update
  basePolRead.io.wIdx := wbStageIdx
  basePolRead.io.wData := wbStageState
  basePolRead.io.fwd := fwd1

  // The assignments are read one cycle later, so they arrive in the update stage. The memory forwards a write that
  // happens in the same cycle as the read. A write in the following cycle causes a bubble, in which the memory reads
  // the set of the request in the update stage again.
  assignArr.io.stall := io.control.stall || insertBubble
  assignArr.io.rSet := idxDelayReg
  assignArr.io.wrEn := wbStageAssignWrEn
  assignArr.io.wrSet := wbStageIdx
  assignArr.io.wrWay := wbStageAssignWay
  assignArr.io.wrLineAssign := wbStageAssignCore

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
  val basePolUpdate = Module(basePolicyType.buildBasePolicyUpdate(nWays))

  val baseCandidates = repSetPipeReg1

  val criticalWays = Wire(Vec(nWays, Bool()))
  for (wayIdx <- 0 until nWays) {
    // LC: a line is critical while it is assigned to a critical core
    criticalWays(wayIdx) := assignArr.io.rValidAssign(wayIdx) && criticalCores(assignArr.io.rLineAssign(wayIdx))
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
  basePolUpdate.io.stateIn := statePipeReg1

  // The WB stage is empty after a bubble
  val repWayPipeReg2 = PipelineReg(repWay, 0.U, !io.control.stall, insertBubble)
  val isValidPipeReg2 = PipelineReg(!reject, true.B, !io.control.stall, insertBubble)
  val repSetPipeReg2 = PipelineReg(baseCandidates, getDefaultRepSet, !io.control.stall, insertBubble)
  val statePipeReg2 = PipelineReg(basePolUpdate.io.stateOut, 0.U, !io.control.stall, insertBubble)
  val setIdxPipeReg2 = PipelineReg(setIdxPipeReg1, 0.U, !io.control.stall, insertBubble)
  val coreIdPipeReg2 = PipelineReg(coreIdPipeReg1, 0.U, !io.control.stall, insertBubble)
  val isHitPipeReg2 = PipelineReg(io.info.isHit, false.B, !io.control.stall, insertBubble)
  val hitWayPipeReg2 = PipelineReg(io.info.hitWay, 0.U, !io.control.stall, insertBubble)
  val isHitWayCritPipeReg2 = PipelineReg(criticalWays(io.info.hitWay), false.B, !io.control.stall, insertBubble)
  val isRepWayCritPipeReg2 = PipelineReg(criticalWays(repWay), false.B, !io.control.stall, insertBubble)

  // ---------------- WB stage ----------------
  // LC: an allocated line is assigned to the requesting core. A critical core claims a non-critical line it hits.
  val isReqCoreCrit = criticalCores(coreIdPipeReg2)
  val claim = if (claimOnHit) io.control.update && isHitPipeReg2 && isReqCoreCrit && !isHitWayCritPipeReg2 else false.B

  wbStageIdx := setIdxPipeReg2
  wbStageState := statePipeReg2
  wbStageAssignWrEn := io.control.evict || claim
  wbStageAssignWay := Mux(io.control.evict, repWayPipeReg2, hitWayPipeReg2)
  wbStageAssignCore := coreIdPipeReg2

  // The state and line assignments of the request in the update stage are stale if the WB stage updates the same set
  insertBubble := reqValidPipeReg1 && io.control.update && wbStageIdx === setIdxPipeReg1

  // Default output assignments
  io.control <> 0.U.asTypeOf(io.control)
  io.info <> 0.U.asTypeOf(io.info)

  io.control.replaceWay := repWayPipeReg2
  io.control.isValid := isValidPipeReg2 // LC: only rejected with rejectNonCritical, a rejected request leaves no state behind
  io.control.insertBubble := insertBubble

  // LC: with priorityQueues, a critical core is always at its limit, so its misses and the write-backs of critical
  // lines are pushed to the priority queues
  io.info.updateCoreIsCrit := isReqCoreCrit
  io.info.updateCoreReachedLimit := priorityQueues.B && isReqCoreCrit
  io.info.isReplacementWayCrit := isRepWayCritPipeReg2
  io.info.isReplacementWayAtLimit := priorityQueues.B

  // Debugging signal
  repSet := repSetPipeReg2
}
