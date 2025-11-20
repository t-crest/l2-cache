package caches.hardware.pipelined.cache

import caches.hardware.pipelined.cache.SharedPipelinedCacheTest._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SharedPipelinedCacheTimeoutTest extends AnyFlatSpec with ChiselScalatestTester {
  "SharedPipelinedCache" should "process pipelined requests for 8 ways, 128 sets, with timeout policy" in {
    val cache = generateDut(CacheConfigs.config64TimeOut)

    test(cache.dutGen.apply()).withAnnotations(Seq(WriteFstAnnotation, VerilatorBackendAnnotation)) { dut =>
      defaultAssignments(dut, cache.nCores)

      // Issue the first set of requests
      performTestActions(
        dut,
        cache.nCores,
        Tests.testActions7,
        cache.indexWidth,
        cache.blockOffsetWidth,
        cache.byteOffsetWidth,
        1500
      )

      dut.clock.step()
    }
  }
}
