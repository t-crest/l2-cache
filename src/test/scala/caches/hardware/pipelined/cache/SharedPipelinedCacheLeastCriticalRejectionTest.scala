package caches.hardware.pipelined.cache

import SharedPipelinedCacheTest._
import SharedPipelinedCacheRequestHelpers._
import caches.hardware.pipelined.SharedPipelinedCacheTestTop
import caches.hardware.reppol.BasePolicies
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
      unsetCoreAsCritical(dut, coreID = 0)
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

      unsetCoreAsCritical(dut, coreID = 0)
      waitForResponse(dut, coreId = 1, reqId = 51)

      issue(dut, coreId = 2, reqId = 71, addr = newLine)
      assert(waitForResponse(dut, coreId = 2, reqId = 71) == writeData)
    }
  }
}
