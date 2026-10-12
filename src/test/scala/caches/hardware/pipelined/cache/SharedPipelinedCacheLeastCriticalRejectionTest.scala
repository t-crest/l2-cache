package caches.hardware.pipelined.cache

import SharedPipelinedCacheTest._
import caches.hardware.pipelined.SharedPipelinedCacheTestTop
import caches.hardware.reppol.{BasePolicies, SchedulerCmd}
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/**
 * End-to-end test of the least critical policy with rejection: a non-critical miss to a set that only holds lines of
 * a critical core is rejected and retried through the rejection queue until the core is no longer critical.
 */
class SharedPipelinedCacheLeastCriticalRejectionTest extends AnyFlatSpec with ChiselScalatestTester {
  private val config = CacheConfigs.config64BitPlru.copy(repPolConfig = LeastCriticalConfiguration(BasePolicies.BIT_PLRU, rejectNonCritical = true))
  private val nWays = config.nWays
  private val set = 5

  private def lineAddr(cache: Dut, tag: Int, index: Int): Int = {
    (tag << (cache.indexWidth + cache.blockOffsetWidth + cache.byteOffsetWidth)) |
      (index << (cache.blockOffsetWidth + cache.byteOffsetWidth))
  }

  /** Issues a request on one cycle and waits until the cache has accepted it. */
  private def issue(dut: SharedPipelinedCacheTestTop, coreId: Int, reqId: Int, addr: Int, write: Option[BigInt] = None): Unit = {
    val req = dut.io.requests.cores(coreId).req

    req.reqId.valid.poke(true.B)
    req.reqId.bits.poke(reqId.U)
    req.addr.poke(addr.U)
    req.rw.poke(write.isDefined.B)
    req.wData.poke(write.getOrElse(BigInt(0)).U)
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

  /** Waits for the response to a request and returns its data. */
  private def waitForResponse(dut: SharedPipelinedCacheTestTop, coreId: Int, reqId: Int, maxCCs: Int = 2000): BigInt = {
    val resp = dut.io.requests.cores(coreId).resp
    var waitedCCs = 0

    while (!(resp.reqId.valid.peekBoolean() && resp.reqId.bits.peek().litValue == reqId)) {
      dut.clock.step()
      waitedCCs += 1
      assert(waitedCCs < maxCCs, s"Core $coreId did not receive a response for request $reqId.")
    }

    val data = resp.rData.peek().litValue
    dut.clock.step()
    data
  }

  private def assertNoResponse(dut: SharedPipelinedCacheTestTop, coreId: Int, cycles: Int): Unit = {
    for (_ <- 0 until cycles) {
      assert(!dut.io.requests.cores(coreId).resp.reqId.valid.peekBoolean(), s"Core $coreId received a response although its request should be rejected.")
      dut.clock.step()
    }
  }

  private def unsetCoreAsCritical(dut: SharedPipelinedCacheTestTop, coreId: Int): Unit = {
    dut.io.scheduler.cmd.poke(SchedulerCmd.RD)
    dut.io.scheduler.addr.poke(coreId.U)
    dut.clock.step()
    dut.io.scheduler.cmd.poke(SchedulerCmd.NULL)
    dut.io.scheduler.addr.poke(0.U)
  }

  /** Core 0 is set critical and fills every way of the set with its own lines. */
  private def fillSetWithCriticalLines(dut: SharedPipelinedCacheTestTop, cache: Dut): Unit = {
    defaultAssignments(dut, cache.nCores)
    setCoreAsCritical(dut, 0, 1)
    dut.clock.step(2)

    for (tag <- 1 to nWays) {
      issue(dut, coreId = 0, reqId = tag, addr = lineAddr(cache, tag, set))
      waitForResponse(dut, coreId = 0, reqId = tag)
    }
  }

  "SharedPipelinedCache with least critical rejection" should "retry a rejected read until the critical core is unset" in {
    val cache = generateDut(config)

    test(cache.dutGen.apply()) { dut =>
      fillSetWithCriticalLines(dut, cache)
      val newLine = lineAddr(cache, nWays + 1, set)

      // Core 1 misses in the full set and is rejected
      issue(dut, coreId = 1, reqId = 50, addr = newLine)
      assertNoResponse(dut, coreId = 1, cycles = 500)

      // The critical core is not blocked by the rejected request
      issue(dut, coreId = 0, reqId = 60, addr = lineAddr(cache, 1, set))
      waitForResponse(dut, coreId = 0, reqId = 60, maxCCs = 50)

      // Once core 0 is no longer critical, the rejected request is served
      unsetCoreAsCritical(dut, coreId = 0)
      val rejectedData = waitForResponse(dut, coreId = 1, reqId = 50)

      // A later read of the same line hits and returns the same data
      issue(dut, coreId = 2, reqId = 70, addr = newLine)
      assert(waitForResponse(dut, coreId = 2, reqId = 70) == rejectedData)
    }
  }

  it should "apply a rejected write only once it is accepted" in {
    val cache = generateDut(config)
    val writeData = BigInt("0123456789abcdef0011223344556677", 16)

    test(cache.dutGen.apply()) { dut =>
      fillSetWithCriticalLines(dut, cache)
      val newLine = lineAddr(cache, nWays + 1, set)

      issue(dut, coreId = 1, reqId = 51, addr = newLine, write = Some(writeData))
      assertNoResponse(dut, coreId = 1, cycles = 500)

      // While the write is rejected, none of core 0's lines was replaced
      for (tag <- 1 to nWays) {
        issue(dut, coreId = 0, reqId = 100 + tag, addr = lineAddr(cache, tag, set))
        waitForResponse(dut, coreId = 0, reqId = 100 + tag, maxCCs = 50)
      }

      unsetCoreAsCritical(dut, coreId = 0)
      waitForResponse(dut, coreId = 1, reqId = 51)

      issue(dut, coreId = 2, reqId = 71, addr = newLine)
      assert(waitForResponse(dut, coreId = 2, reqId = 71) == writeData)
    }
  }
}
