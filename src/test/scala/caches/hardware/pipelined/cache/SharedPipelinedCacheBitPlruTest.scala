package caches.hardware.pipelined.cache

import caches.hardware.pipelined.cache.SharedPipelinedCacheTest._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SharedPipelinedCacheBitPlruTest extends AnyFlatSpec with ChiselScalatestTester {
  "SharedPipelinedCache" should "process pipelined requests for 8 ways, 128 sets, with bit plru policy" in {
    val cache = generateDut(CacheConfigs.config64BitPlru)

    test(cache.dutGen.apply()).withAnnotations(Seq(WriteFstAnnotation, VerilatorBackendAnnotation)) { dut =>
      defaultAssignments(dut, cache.nCores)

      performTestActions(
        dut,
        cache.nCores,
        Tests.testActions1,
        cache.indexWidth,
        cache.blockOffsetWidth,
        cache.byteOffsetWidth,
        700
      )

      dut.clock.step()
    }
  }

  "SharedPipelinedCache" should "work with mshr entries that are full of cmds" in {
    val cache = generateDut(CacheConfigs.config64BitPlru)

    test(cache.dutGen.apply()).withAnnotations(Seq(WriteFstAnnotation, VerilatorBackendAnnotation)) { dut =>
      defaultAssignments(dut, cache.nCores)

      // Issue the first set of requests
      performTestActions(
        dut,
        cache.nCores,
        Tests.testActions5,
        cache.indexWidth,
        cache.blockOffsetWidth,
        cache.byteOffsetWidth,
        150
      )

      dut.clock.step()
    }
  }

  "SharedPipelinedCache" should "circumvent pipeline hazards" in {
    val cache = generateDut(CacheConfigs.config64BitPlru)

    test(cache.dutGen.apply()).withAnnotations(Seq(WriteFstAnnotation, VerilatorBackendAnnotation)) { dut =>
      defaultAssignments(dut, cache.nCores)

      // Issue the first set of requests
      performTestActions(
        dut,
        cache.nCores,
        Tests.testActions9,
        cache.indexWidth,
        cache.blockOffsetWidth,
        cache.byteOffsetWidth,
        1000
      )

      dut.clock.step()
    }
  }
}
