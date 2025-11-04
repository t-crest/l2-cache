package caches.hardware.pipelined.cache

import caches.hardware.pipelined.cache.SharedPipelinedCacheTest._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SharedPipelinedCacheContWithAllEventsTest extends AnyFlatSpec with ChiselScalatestTester {
  "SharedPipelinedCache" should "work with all contention events" in {
    val cache = generateDut(CacheConfigs.config64ContMimPrecWb)

    test(cache.dutGen.apply()).withAnnotations(Seq(WriteFstAnnotation, VerilatorBackendAnnotation)) { dut =>
      defaultAssignments(dut, cache.nCores)

      // Issue the first set of requests
      performTestActions(
        dut,
        cache.nCores,
        Tests.testActions8,
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
