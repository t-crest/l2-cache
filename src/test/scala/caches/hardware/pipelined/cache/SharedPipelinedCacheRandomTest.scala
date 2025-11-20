package caches.hardware.pipelined.cache

import caches.hardware.pipelined.cache.SharedPipelinedCacheTest._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

class SharedPipelinedCacheRandomTest extends AnyFlatSpec with ChiselScalatestTester {
  def generateRandomTestCase(nCores: Int, maxTag: Int, maxIdx: Int, nSubBlocks: Int, bytesPerBlock: Int, bytesPerSubBlock: Int, memFile: String, setCore0Crit: Boolean = false, testActionsLen: Int = 100): Array[TestAction] = {
    val rand = new Random()

    var testActions = Array.empty[TestAction]
    val writes = mutable.Set[(Int, Int, Int, String)]()
    val avgIdx = rand.nextInt(maxIdx)

    if (setCore0Crit) {
      // Cannot set contention limit, since the rejection queue can lose requests
      // testActions = testActions :+ PerformSchedulerOperation(0, true, Some(5 + rand.nextInt(20)))
    }

    for (i <- 0 until testActionsLen) {
      val tag = rand.nextInt(maxTag)
      val idx = drawIntFromNormalDist(rand, avgIdx, 5, 0, maxIdx - 1)
      val blockOff = rand.nextInt(nSubBlocks - 1)

      val coreId = rand.nextInt(nCores - 1)
      val rw = rand.nextBoolean()

      var wData = ""
      val maxWordVal = math.pow(2, 32).toInt
      for (i <- 0 until (bytesPerSubBlock / 4)) {
        val randWriteVal = rand.nextInt(maxWordVal)
        wData += f"$randWriteVal%08x"
      }

      val readAfterWrite = writes.find(write => write._1 === tag && write._2 === idx && write._3 === blockOff)
      var expectedData = ""
      if (readAfterWrite.isDefined) {
        expectedData = readAfterWrite.get._4
      } else {
        expectedData = getExpectedData(tag, idx, blockOff, maxIdx, bytesPerBlock, bytesPerSubBlock, memFile)
      }

      val action = CacheRequest(coreId = coreId, reqId = i, rw = rw, tag = tag, index = idx, blockOffset = blockOff, wData = Some("h" + wData), expectedData = Some(expectedData))
      testActions = testActions :+ action

      if (rw) {
        writes.add((tag, idx, blockOff, wData))
      }
    }

    if (setCore0Crit) {
      testActions = testActions :+ PerformSchedulerOperation(0, false)
      testActions = testActions :+ Stall(500)
    }

    testActions
  }

  def drawIntFromNormalDist(gen: Random, mean: Double, stdDev: Double, min: Int = 0, max: Int = 127): Int = {
    val value = mean + stdDev * gen.nextGaussian()
    val intValue = Math.round(value).toInt
    intValue.max(min).min(max)
  }

  def getExpectedData(tag: Int, index: Int, blockOffset: Int, nSets: Int, bytesPerBlock: Int, bytesPerSubBlock: Int, memFile: String, memFileAlignment: Int = 32): String = {
    if (memFileAlignment % 8 !== 0) {
      throw new IllegalArgumentException("Memory file alignment is not a multiple of 8.")
    }

    val bytesPerFileLine = memFileAlignment / 8

    val linesPerSubBlock = bytesPerSubBlock / bytesPerFileLine
    val linesPerSet = (bytesPerBlock / bytesPerSubBlock) * linesPerSubBlock
    val linesPerTag = nSets * linesPerSet

    val readLineNumber = (tag * linesPerTag) + (index * linesPerSet) + (blockOffset * linesPerSubBlock)

    val source = Source.fromFile(memFile)
    val lines = source.getLines().drop(readLineNumber)

    var data = ""

    for (_ <- 0 until linesPerSubBlock) {
      val readLine = lines.next()
      data = readLine + data
    }

    source.close()

    data
  }

  "SharedPipelinedCache" should "process random requests for bit plru policy" in {
    val cacheConfig = CacheConfigs.config64BitPlru
    val nSubBlocks = cacheConfig.bytesPerBlock / cacheConfig.bytesPerSubBlock

    val cache = generateDut(cacheConfig)

    val testActions = generateRandomTestCase(
      cacheConfig.nCores,
      300, // Setting maximum tag to 300 since the hex file contains no data at higher tags
      math.pow(2, cache.indexWidth).toInt,
      nSubBlocks,
      cacheConfig.bytesPerBlock,
      cacheConfig.bytesPerSubBlock,
      cacheConfig.memFile.getOrElse("")
    )

    test(cache.dutGen.apply()).withAnnotations(Seq(WriteFstAnnotation, VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(5000)

      defaultAssignments(dut, cache.nCores)

      performTestActions(
        dut,
        cache.nCores,
        testActions,
        cache.indexWidth,
        cache.blockOffsetWidth,
        cache.byteOffsetWidth,
        35500
      )

      dut.clock.step()
    }
  }

  "SharedPipelinedCache" should "process random requests for contention policy" in {
    val cacheConfig = CacheConfigs.config64ContMimPrecWb
    val nSubBlocks = cacheConfig.bytesPerBlock / cacheConfig.bytesPerSubBlock

    val cache = generateDut(cacheConfig)

    val testActions = generateRandomTestCase(
      cacheConfig.nCores,
      300, // Setting maximum tag to 300 since the hex file contains no data at higher tags
      math.pow(2, cache.indexWidth).toInt,
      nSubBlocks,
      cacheConfig.bytesPerBlock,
      cacheConfig.bytesPerSubBlock,
      cacheConfig.memFile.getOrElse(""),
      setCore0Crit = true
    )

    test(cache.dutGen.apply()).withAnnotations(Seq(WriteFstAnnotation, VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(5000)

      defaultAssignments(dut, cache.nCores)

      performTestActions(
        dut,
        cache.nCores,
        testActions,
        cache.indexWidth,
        cache.blockOffsetWidth,
        cache.byteOffsetWidth,
        3500
      )

      dut.clock.step()
    }
  }
}
