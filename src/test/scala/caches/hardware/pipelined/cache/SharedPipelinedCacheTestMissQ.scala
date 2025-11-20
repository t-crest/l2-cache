package caches.hardware.pipelined.cache

import caches.hardware.pipelined.cache.SharedPipelinedCacheTest._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SharedPipelinedCacheTestMissQ extends AnyFlatSpec with ChiselScalatestTester {
  "SharedPipelinedCache" should "work with miss-q and precedent events for 8 ways and 128 sets" in {
    val cache = generateDut(CacheConfigs.config64ContMimPrec)

    test(cache.dutGen.apply()).withAnnotations(Seq(WriteFstAnnotation, VerilatorBackendAnnotation)) { dut =>
      defaultAssignments(dut, cache.nCores)

      // Issue the first set of requests
      performTestActions(
        dut,
        cache.nCores,
        Tests.testActions4,
        cache.indexWidth,
        cache.blockOffsetWidth,
        cache.byteOffsetWidth,
        1000
      )

      dut.clock.step()
    }
  }
}
