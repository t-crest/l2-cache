package caches.hardware.pipelined.cache

import SharedPipelinedCacheTest._
import caches.hardware.pipelined.SharedPipelinedCacheTestTop
import caches.hardware.reppol.SchedulerCmd
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/**
 * End-to-end test of the queue related contention events (miss-in-miss and miss-queue events).
 *
 * When a critical core misses, it has to wait for all the misses of other cores that are already queued. Each of them
 * must be subtracted from the critical core's remaining contention budget. Misses of cores that are critical
 * themselves must not be counted: contention between jobs of the same criticality is not tracked.
 *
 * The remaining budget is observed through the scheduler interface: reading a core's entry returns the remaining
 * contention limit and clears the core's critical flag.
 */
class SharedPipelinedCacheContentionCountTest extends AnyFlatSpec with ChiselScalatestTester {
  private val contentionLimit = 100
  private val observedCore = 0

  private def lineAddr(cache: Dut, tag: Int, index: Int): Int = {
    (tag << (cache.indexWidth + cache.blockOffsetWidth + cache.byteOffsetWidth)) |
      (index << (cache.blockOffsetWidth + cache.byteOffsetWidth))
  }

  /** Issues a read request on one cycle and waits until the cache has accepted it. */
  private def issueRead(dut: SharedPipelinedCacheTestTop, coreId: Int, reqId: Int, addr: Int): Unit = {
    val req = dut.io.requests.cores(coreId).req

    req.reqId.valid.poke(true.B)
    req.reqId.bits.poke(reqId.U)
    req.addr.poke(addr.U)
    req.rw.poke(false.B)
    req.wData.poke(0.U)
    req.byteEn.poke(((BigInt(1) << (dut.subBlockDataWidth / 8)) - 1).U)

    var waitedCCs = 0
    while (!req.reqId.ready.peekBoolean()) {
      dut.clock.step()
      waitedCCs += 1
      assert(waitedCCs < 1000, s"Core $coreId's request was never accepted.")
    }

    dut.clock.step()
    req.reqId.valid.poke(false.B)
  }

  private def waitForResponse(dut: SharedPipelinedCacheTestTop, coreId: Int, reqId: Int, maxCCs: Int = 2000): Unit = {
    val resp = dut.io.requests.cores(coreId).resp.reqId
    var waitedCCs = 0

    while (!(resp.valid.peekBoolean() && resp.bits.peek().litValue == reqId)) {
      dut.clock.step()
      waitedCCs += 1
      assert(waitedCCs < maxCCs, s"Core $coreId did not receive a response for request $reqId.")
    }
  }

  /** Returns the remaining contention limit of a core. Also unsets the core as critical. */
  private def readRemainingLimit(dut: SharedPipelinedCacheTestTop, coreId: Int): Int = {
    dut.io.scheduler.cmd.poke(SchedulerCmd.RD)
    dut.io.scheduler.addr.poke(coreId.U)
    val remaining = dut.io.scheduler.rData.peek().litValue.toInt

    dut.clock.step()
    dut.io.scheduler.cmd.poke(SchedulerCmd.NULL)
    dut.io.scheduler.addr.poke(0.U)

    remaining
  }

  /**
   * Makes the given cores critical, lets the cores in 'coresAhead' queue a miss (one per cycle, each to a different
   * set), and then lets the observed core miss. Returns the observed core's remaining contention limit.
   */
  private def remainingLimitAfterMiss(criticalCores: Seq[Int], coresAhead: Seq[Int]): Int = {
    val cache = generateDut(CacheConfigs.config64ContMimPrecWb)
    var remaining = -1

    test(cache.dutGen.apply()) { dut =>
      defaultAssignments(dut, cache.nCores)

      for (core <- criticalCores) {
        setCoreAsCritical(dut, core, contentionLimit)
      }
      dut.clock.step(2)

      // The misses are slow enough (a full line is fetched from memory) that all of them are still queued when the
      // observed core's miss reaches the replacement stage
      for ((core, i) <- coresAhead.zipWithIndex) {
        issueRead(dut, core, reqId = i, addr = lineAddr(cache, tag = 1, index = 10 * (i + 1)))
      }

      issueRead(dut, observedCore, reqId = 100, addr = lineAddr(cache, tag = 1, index = 100))
      waitForResponse(dut, observedCore, reqId = 100)
      dut.clock.step(20)

      remaining = readRemainingLimit(dut, observedCore)
    }

    remaining
  }

  "SharedPipelinedCache with contention policy" should "not charge a critical core when no other miss is queued" in {
    val remaining = remainingLimitAfterMiss(criticalCores = Seq(0), coresAhead = Seq())
    assert(remaining == contentionLimit, s"Remaining limit is $remaining, expected the unchanged limit $contentionLimit.")
  }

  it should "charge a critical core for every queued miss of a non-critical core" in {
    val remaining = remainingLimitAfterMiss(criticalCores = Seq(0), coresAhead = Seq(1, 2, 3))
    assert(remaining == contentionLimit - 3, s"Remaining limit is $remaining, expected ${contentionLimit - 3} after three queued non-critical misses.")
  }

  it should "not charge a critical core for queued misses of other critical cores" in {
    // Cores 2 and 3 are non-critical, core 1 is critical like the observed core 0
    val remaining = remainingLimitAfterMiss(criticalCores = Seq(0, 1), coresAhead = Seq(2, 3, 1))
    assert(remaining == contentionLimit - 2, s"Remaining limit is $remaining, expected ${contentionLimit - 2}: only the two non-critical misses count.")
  }
}
