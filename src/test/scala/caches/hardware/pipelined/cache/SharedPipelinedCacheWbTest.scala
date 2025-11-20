package caches.hardware.pipelined.cache

import caches.hardware.pipelined.cache.SharedPipelinedCacheTest._
import chisel3.stage.PrintFullStackTraceAnnotation
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SharedPipelinedCacheWbTest extends AnyFlatSpec with ChiselScalatestTester {
  "SharedPipelinedCache" should "work with wb contention events" in {
    val cache = generateDut(CacheConfigs.config64ContWb)

    test(cache.dutGen.apply()).withAnnotations(Seq(WriteFstAnnotation, VerilatorBackendAnnotation, PrintFullStackTraceAnnotation)) { dut =>
      defaultAssignments(dut, cache.nCores)

      // Issue the first set of requests
      performTestActions(
        dut,
        cache.nCores,
        Tests.testActions6,
        cache.indexWidth,
        cache.blockOffsetWidth,
        cache.byteOffsetWidth,
        1500
      )

      dut.clock.step()
    }
  }
}
