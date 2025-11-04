package caches.hardware.pipelined.cache

import caches.hardware.pipelined.cache.SharedPipelinedCacheTest._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SharedPipelinedCacheStressTest extends AnyFlatSpec with ChiselScalatestTester {
  "SharedPipelinedCache" should "handle stress test with bit plru policy" in {
    val cache = generateDut(CacheConfigs.config64BitPlru)

    test(cache.dutGen.apply()).withAnnotations(Seq(WriteFstAnnotation, VerilatorBackendAnnotation)) { dut =>
      defaultAssignments(dut, cache.nCores)

      performTestActions(
        dut,
        cache.nCores,
        Tests.testActions3,
        cache.indexWidth,
        cache.blockOffsetWidth,
        cache.byteOffsetWidth,
        1000,
        printResults = PRINT_RESULTS
      )

      dut.clock.step()
    }
  }
}
