package caches.hardware.pipelined.cache

import caches.hardware.pipelined.SharedPipelinedCacheTestTop
import caches.hardware.reppol._
import chisel3._
import chisel3.util.log2Up
import chiseltest._

import scala.collection.mutable

trait TestAction {}

trait PolicyConfiguration {}

case class CacheRequest(
                         coreId: Int,
                         reqId: Int,
                         rw: Boolean,
                         tag: Int,
                         index: Int,
                         blockOffset: Int,
                         byteOffset: Int = 0,
                         rejected: Boolean = false,
                         byteEn: Option[String] = None,
                         wData: Option[String] = None,
                         expectedData: Option[String] = None
                       ) extends TestAction

case class Stall(stallCycles: Int = 0) extends TestAction()

case class ExpectFinishedRejectedResponse(coreId: Int, reqId: Int, expectedData: String) extends TestAction()

case class PerformSchedulerOperation(addr: Int, rw: Boolean, wData: Option[Int] = None) extends TestAction()

case class CacheResponse(
                          receivedCC: Int,
                          coreId: Int,
                          reqId: Int,
                          data: String
                        )

case class CacheConfiguration(
                               sizeInBytes: Int,
                               nCores: Int,
                               nWays: Int,
                               addressWidth: Int,
                               reqIdWidth: Int,
                               bytesPerBlock: Int,
                               bytesPerSubBlock: Int,
                               repPolConfig: PolicyConfiguration,
                               memBeatSize: Int,
                               memBurstLen: Int,
                               repSetFormat: Option[BaseReplacementSetFormat],
                               memFile: Option[String],
                               nHalfMissCmds: Option[Int] = None
                             )

case class Dut(dutGen: () => SharedPipelinedCacheTestTop, nCores: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, byteOffsetWidth: Int)

case class BitPlruConfiguration() extends PolicyConfiguration

case class TreePlruConfiguration() extends PolicyConfiguration

case class ContentionConfiguration(base: BasePolicyType, mim: Boolean = false, precedent: Boolean = false, wb: Boolean = false) extends PolicyConfiguration

case class TimeoutConfiguration(base: BasePolicyType) extends PolicyConfiguration

object Tests {
  // Test actions for bit and tree PLRUs
  val testActions1: Array[TestAction] = Array(
    CacheRequest(coreId = 1, reqId = 0, tag = 0, index = 0, blockOffset = 0, rw = false, expectedData = Some("cafebabebabecafedeadbeefbeefdead")), // (tag = 0, idx = 0, blockOff = 0, way = 0)
    CacheRequest(coreId = 1, reqId = 1, tag = 0, index = 0, blockOffset = 1, rw = false, expectedData = Some("deadbabebabedeadfeedfacefacefeed")), // (HIT)
    CacheRequest(coreId = 1, reqId = 2, tag = 0, index = 0, blockOffset = 2, rw = true, wData = Some("hd00dfeed0000000000000000"), byteEn = Some("b0000011000000000")), // (HIT)
    CacheRequest(coreId = 3, reqId = 3, tag = 3, index = 55, blockOffset = 0, rw = true, wData = Some("h000000000000000000000000deadba55"), byteEn = Some("b0000000000001111")),
    CacheRequest(coreId = 3, reqId = 4, tag = 3, index = 55, blockOffset = 0, rw = true, wData = Some("h0000000000000000ba55d00d00000000"), byteEn = Some("b0000000011110000")),
    CacheRequest(coreId = 3, reqId = 5, tag = 3, index = 55, blockOffset = 0, rw = true, wData = Some("h00000000f00df17e0000000000000000"), byteEn = Some("b0000111100000000")),
    CacheRequest(coreId = 3, reqId = 6, tag = 3, index = 55, blockOffset = 0, rw = true, wData = Some("hb17ebabe0000000000000000deadba55"), byteEn = Some("b1111000000000000")),
    CacheRequest(coreId = 3, reqId = 7, tag = 0, index = 2, blockOffset = 3, rw = false, expectedData = Some("6a8fdc4ede91ab230fabc9877c3a12ed")), // (tag = 0, idx = 2, blockOff = 3, way = 0)
    CacheRequest(coreId = 2, reqId = 8, tag = 2, index = 0, blockOffset = 0, rw = false, expectedData = Some("013bb292b95895f88cde7faf55adaaba")), // (tag = 2, idx = 0, blockOff = 0, way = 1)
    CacheRequest(coreId = 2, reqId = 9, tag = 2, index = 0, blockOffset = 1, rw = true, wData = Some("h01234567"), byteEn = Some("b0000000000001111")),
    CacheRequest(coreId = 2, reqId = 10, tag = 2, index = 0, blockOffset = 1, rw = true, wData = Some("h09abcdef00000000"), byteEn = Some("b0000000011110000")),
    CacheRequest(coreId = 3, reqId = 11, tag = 3, index = 0, blockOffset = 3, rw = true, wData = Some("hbeefdead000000000000000000000000"), byteEn = Some("b0101000000000000")), // (tag = 3, idx = 0, blockOff = 3, way = 2)
    CacheRequest(coreId = 0, reqId = 12, tag = 4, index = 0, blockOffset = 0, rw = false, expectedData = Some("7c7f2e45dab3dea3508d8aeec21e6aea")), // (tag = 4, idx = 0, blockOff = 0, way = 3)
    CacheRequest(coreId = 1, reqId = 13, tag = 5, index = 0, blockOffset = 1, rw = false, expectedData = Some("cc5fd511c237a27e2451003dbc4d8025")), // (tag = 5, idx = 0, blockOff = 1, way = 4)
    CacheRequest(coreId = 1, reqId = 14, tag = 3, index = 55, blockOffset = 2, rw = false, expectedData = Some("2fbcdc49bd5651eb531b4ef64e1393ed")),
    CacheRequest(coreId = 2, reqId = 15, tag = 6, index = 0, blockOffset = 2, rw = false, expectedData = Some("b58d852edb0bf08973f52e36fcbf3fc4")), // (tag = 6, idx = 0, blockOff = 2, way = 5)
    CacheRequest(coreId = 3, reqId = 16, tag = 3, index = 55, blockOffset = 0, rw = false, expectedData = Some("b17ebabef00df17eba55d00ddeadba55")),
    CacheRequest(coreId = 3, reqId = 17, tag = 7, index = 0, blockOffset = 3, rw = false, expectedData = Some("ab59ff12bd819a95cf599088f0b54814")), // (tag = 7, idx = 0, blockOff = 3, way = 6)
    CacheRequest(coreId = 0, reqId = 18, tag = 8, index = 0, blockOffset = 1, rw = false, expectedData = Some("094b46853d585cb5c0d488396f1c65fd")), // (tag = 8, idx = 0, blockOff = 1, way = 7)
    CacheRequest(coreId = 1, reqId = 19, tag = 0, index = 0, blockOffset = 1, rw = false, expectedData = Some("deadbabebabedeadfeedfacefacefeed")), // (HIT)
    CacheRequest(coreId = 1, reqId = 20, tag = 3, index = 0, blockOffset = 3, rw = false, expectedData = Some("ddef31ad9e4ec895a20047db0a108d2d")), // (HIT)
    CacheRequest(coreId = 1, reqId = 21, tag = 9, index = 0, blockOffset = 0, rw = false, expectedData = Some("3e653a9dbf432147e4d0eef71a9d9897")), // (tag = 0, idx = 0, blockOff = 0, way = 0) Evict way 1
    CacheRequest(coreId = 3, reqId = 22, tag = 3, index = 79, blockOffset = 2, rw = true, wData = Some("hfaceb00c0000000000000000"), byteEn = Some("b0000111100000000")), // (tag = 3, idx = 79, blockOff = 2, way = 0)
    CacheRequest(coreId = 1, reqId = 23, tag = 10, index = 0, blockOffset = 2, rw = false, expectedData = Some("0cae4f56f5aad7083c53d5a9edc9e37c")), // (tag = 0, idx = 0, blockOff = 2, way = 1) Evict way 2
    CacheRequest(coreId = 1, reqId = 24, tag = 2, index = 0, blockOffset = 1, rw = false, expectedData = Some("1338e010da28bd0209abcdef01234567")), // Test the writeback, (tag = 0, idx = 0, blockOff = 0, way = 2) Evict way 2
    // Write to an existing line in the cache (HIT)
    CacheRequest(coreId = 1, reqId = 25, tag = 0, index = 2, blockOffset = 0, rw = true, wData = Some("hd00dfeed"), byteEn = Some("b0000000000001111")), // (tag = 0, idx = 0, blockOff = 2, way = ???)
    // MISS: WAY - 0, INDEX - 45, TAG - 2
    CacheRequest(coreId = 3, reqId = 26, tag = 2, index = 45, blockOffset = 1, rw = true, wData = Some("hfaceb00c00000000"), byteEn = Some("b0000000011110000")), // (tag = 2, idx = 45, blockOff = 1, way = ???)
    // MISS: WAY - 0, INDEX - 159, TAG - 5
    CacheRequest(coreId = 2, reqId = 27, tag = 5, index = 159, blockOffset = 2, rw = true, wData = Some("hcafeface0000000000000000"), byteEn = Some("b0000111100000000")), // (tag = 5, idx = 159, blockOff = 1, way = ???)
    // HIT: WAY - 1, INDEX - 0, TAG - 2
    CacheRequest(coreId = 0, reqId = 28, tag = 2, index = 0, blockOffset = 1, rw = false, expectedData = Some("1338e010da28bd0209abcdef01234567")),
    // MISS: WAY - 0, INDEX - 127, TAG - 8
    CacheRequest(coreId = 0, reqId = 29, tag = 8, index = 127, blockOffset = 3, rw = true, wData = Some("hb00cface000000000000000000000000"), byteEn = Some("b1111000000000000")),
    // MISS: WAY - 1, INDEX - 127, TAG - 5
    CacheRequest(coreId = 0, reqId = 30, tag = 5, index = 127, blockOffset = 1, rw = true, wData = Some("hd15ea55500000000"), byteEn = Some("b0000000011110000")),
    // MISS: WAY - 2, INDEX - 127, TAG - 7
    CacheRequest(coreId = 3, reqId = 31, tag = 7, index = 127, blockOffset = 1, rw = false, expectedData = Some("68667763d4de48b2ec721696cf7b782f")),
    // MISS: WAY - 3, INDEX - 127, TAG - 13
    CacheRequest(coreId = 1, reqId = 32, tag = 13, index = 127, blockOffset = 3, rw = true, wData = Some("hfee1dead000000000000000000000000"), byteEn = Some("b1111000000000000")),
    // MISS AND EVICT: WAY - 0, INDEX - 127, TAG - 11
    CacheRequest(coreId = 0, reqId = 33, tag = 11, index = 127, blockOffset = 1, rw = true, wData = Some("hdeadfee100000000"), byteEn = Some("b0000000011110000")),
    //MISS AND EVICT: WAY - 0, INDEX - 127, TAG - 11
    CacheRequest(coreId = 0, reqId = 34, tag = 11, index = 127, blockOffset = 2, rw = false, expectedData = Some("a8f2ed21c6a55c1d9051b72d6422762a")),
    CacheRequest(coreId = 0, reqId = 35, tag = 221, index = 62, blockOffset = 0, rw = false, expectedData = Some("1bc046d6a45fd8ac65676a660e8c3047")),
    CacheRequest(coreId = 1, reqId = 36, tag = 221, index = 62, blockOffset = 0, rw = false, expectedData = Some("1bc046d6a45fd8ac65676a660e8c3047")),
    CacheRequest(coreId = 2, reqId = 37, tag = 221, index = 62, blockOffset = 0, rw = false, expectedData = Some("1bc046d6a45fd8ac65676a660e8c3047")),
    CacheRequest(coreId = 3, reqId = 38, tag = 221, index = 62, blockOffset = 0, rw = false, expectedData = Some("1bc046d6a45fd8ac65676a660e8c3047")),
    Stall(stallCycles = 100),
    CacheRequest(coreId = 0, reqId = 39, tag = 16, index = 113, blockOffset = 0, rw = false, expectedData = Some("a4d868e8605871a722f93960ce9195c6")),
    Stall(stallCycles = 21),
    CacheRequest(coreId = 0, reqId = 40, tag = 16, index = 113, blockOffset = 1, rw = false, expectedData = Some("18d3be76d8a863e9b85207e7cac5155b")),
    CacheRequest(coreId = 0, reqId = 41, tag = 16, index = 113, blockOffset = 2, rw = false, expectedData = Some("358958f999ddee22de0082b374f1b3f5")),
  )

  // Test actions for contention cache
  val testActions2: Array[TestAction] = Array(
    PerformSchedulerOperation(addr = 1, rw = true, wData = Some(2)),
    PerformSchedulerOperation(addr = 3, rw = true, wData = Some(10)),
    CacheRequest(coreId = 1, reqId = 0, tag = 0, index = 0, blockOffset = 0, rw = false, expectedData = Some("cafebabebabecafedeadbeefbeefdead")), // Bring new line into the cache (put in way: 0, idx: 0)
    CacheRequest(coreId = 1, reqId = 1, tag = 0, index = 4, blockOffset = 0, rw = false, expectedData = Some("8fb2741c9dea0137bc3f0e2a40cbde98")), // Bring new line into the cache (put in way: 0, idx: 4)
    CacheRequest(coreId = 1, reqId = 2, tag = 1, index = 0, blockOffset = 0, rw = false, expectedData = Some("bbef1226751129196ede4c8a9dc4fbd4")), // Bring new line into the cache (put in way: 1, idx: 0)
    CacheRequest(coreId = 3, reqId = 3, tag = 2, index = 0, blockOffset = 0, rw = false, expectedData = Some("013bb292b95895f88cde7faf55adaaba")), // Bring new line into the cache (put in way: 2, idx: 0)
    CacheRequest(coreId = 1, reqId = 4, tag = 0, index = 0, blockOffset = 1, rw = false, expectedData = Some("deadbabebabedeadfeedfacefacefeed")), // (HIT)
    CacheRequest(coreId = 1, reqId = 5, tag = 0, index = 0, blockOffset = 2, rw = true, wData = Some("hd00dfeed"), byteEn = Some("b0000000000001111")), // Write to an existing line in the cache (HIT)
    CacheRequest(coreId = 1, reqId = 6, tag = 0, index = 4, blockOffset = 1, rw = false, expectedData = Some("ce28f9010fdabe34b93e1a7d7ac4d2f1")), // (HIT)
    CacheRequest(coreId = 1, reqId = 7, tag = 1, index = 0, blockOffset = 3, rw = false, expectedData = Some("107709677fdfcc120da699175fad2fc1")), // (HIT)
    CacheRequest(coreId = 1, reqId = 8, tag = 2, index = 4, blockOffset = 1, rw = false, expectedData = Some("a2cc291a987d1f7c4d0f31f40e755553")), // Bring new line into the cache (put in way: 1, idx: 4)
    CacheRequest(coreId = 3, reqId = 9, tag = 3, index = 0, blockOffset = 2, rw = false, expectedData = Some("c70485594aeb67d9904d7f5fd0cbca8d")), // Bring new line into the cache (put in way: 3, idx: 0)
    CacheRequest(coreId = 3, reqId = 10, tag = 3, index = 4, blockOffset = 3, rw = false, expectedData = Some("52c626535c92c204ebcdf992fdc70d3f")), // Bring new line into the cache (put in way: 2, idx: 4)
    CacheRequest(coreId = 3, reqId = 11, tag = 1, index = 4, blockOffset = 3, rw = false, expectedData = Some("ba8c556a6162ff795bcfa1e8a6c78ad4")), // Bring new line into the cache (put in way: 3, idx: 4)
    CacheRequest(coreId = 1, reqId = 12, tag = 0, index = 3, blockOffset = 2, rw = false, expectedData = Some("56789abcef01234593cd78aba1e2f8d0")), // Bring new line into the cache (put in way: 0, idx: 3)
    CacheRequest(coreId = 2, reqId = 13, tag = 0, index = 2, blockOffset = 3, rw = false, expectedData = Some("6a8fdc4ede91ab230fabc9877c3a12ed")), // Bring new line into the cache (put in way: 0, idx: 2)
    CacheRequest(coreId = 3, reqId = 14, tag = 4, index = 0, blockOffset = 1, rw = false, expectedData = Some("734ccecdbe11b44dae6eaf60cb218892")), // Bring new line into the cache (put in way: 4, idx: 0)
    CacheRequest(coreId = 0, reqId = 15, tag = 4, index = 4, blockOffset = 3, rw = false, expectedData = Some("640df405821ff856a0436b31f624cd8b")), // (HIT)
    CacheRequest(coreId = 3, reqId = 16, tag = 0, index = 0, blockOffset = 2, rw = false, expectedData = Some("fee1deaddeadfee1c00010ffd00dfeed")), // (HIT)
    CacheRequest(coreId = 1, reqId = 17, tag = 5, index = 0, blockOffset = 1, rw = false, expectedData = Some("cc5fd511c237a27e2451003dbc4d8025")), // Bring new line into the cache (put in way: 5, idx: 0),
    CacheRequest(coreId = 1, reqId = 18, tag = 1, index = 3, blockOffset = 1, rw = false, expectedData = Some("30464e5bf598385749afdfcfba3bae98")), // Bring new line into the cache (put in way: 1, idx: 3),
    CacheRequest(coreId = 1, reqId = 19, tag = 5, index = 4, blockOffset = 3, rw = false, expectedData = Some("07b14887beaa396de0919c32e9127550")), // Bring new line into the cache (put in way: 4, idx: 4),
    CacheRequest(coreId = 1, reqId = 20, tag = 6, index = 0, blockOffset = 0, rw = false, expectedData = Some("2888e103997223a9003bc584e091cc8a")), // Bring new line into the cache (put in way: 6, idx: 0),
    CacheRequest(coreId = 3, reqId = 21, tag = 6, index = 4, blockOffset = 1, rw = false, expectedData = Some("3bc5171128c41fbfe21a727455ae40f9")), // Bring new line into the cache (put in way: 5, idx: 4),
    CacheRequest(coreId = 3, reqId = 22, tag = 7, index = 0, blockOffset = 1, rw = false, expectedData = Some("040fd41d7771f0535a07ec451db97efb")), // Bring new line into the cache (put in way: 7, idx: 0),
    CacheRequest(coreId = 0, reqId = 23, tag = 8, index = 0, blockOffset = 0, rw = false, expectedData = Some("39df6c998739192bae26debd84620423")), // Bring new line into the cache (put in way: 0, idx: 0),
    CacheRequest(coreId = 2, reqId = 24, tag = 9, index = 0, blockOffset = 0, rw = false, expectedData = Some("3e653a9dbf432147e4d0eef71a9d9897")), // Bring new line into the cache (put in way: 1, idx: 0) reach contention limit for core 1,
    CacheRequest(coreId = 0, reqId = 25, tag = 10, index = 0, blockOffset = 0, rw = false, expectedData = Some("af675aee062fe9dc7fcb9bda56a92dbc")), // Bring new line into the cache (put in way: 2, idx: 0),
    CacheRequest(coreId = 2, reqId = 26, tag = 11, index = 0, blockOffset = 0, rw = false, expectedData = Some("03ca3d2c8469be21134a3cb1702b246e")), // Bring new line into the cache (put in way: 2, idx: 0),
    CacheRequest(coreId = 1, reqId = 27, tag = 12, index = 0, blockOffset = 0, rw = false, expectedData = Some("56fc51c6be6e2e632bba17208a3b9cec")), // Bring new line into the cache (put in way: 0, idx: 0),
    CacheRequest(coreId = 3, reqId = 28, tag = 13, index = 0, blockOffset = 0, rw = false, expectedData = Some("b97d19f32ca140ba54915eb145da4782")), // Bring new line into the cache (put in way: 1, idx: 0),
    CacheRequest(coreId = 3, reqId = 29, tag = 14, index = 0, blockOffset = 0, rw = false, expectedData = Some("117e27b41fb7ac5b8ed5184c7f9453ae")), // Bring new line into the cache (put in way: 2, idx: 0),
    CacheRequest(coreId = 3, reqId = 30, tag = 15, index = 0, blockOffset = 0, rw = false, expectedData = Some("dd27e4c14bf57c1ca196400c0e5d5ce7")), // Bring new line into the cache (put in way: 3, idx: 0),
    CacheRequest(coreId = 2, reqId = 31, tag = 16, index = 0, blockOffset = 0, rw = false, expectedData = Some("40a13302ac635051b398eb9a4cec416c"), rejected = true), // Rejected response (once free, put in way: 4, idx: 0),
    CacheRequest(coreId = 0, reqId = 32, tag = 17, index = 0, blockOffset = 0, rw = false, expectedData = Some("d1f8fbeb63d88f19a2af35a0cd00f5ef"), rejected = true), // Rejected response (once free, put in way: 4, idx: 0),
    CacheRequest(coreId = 1, reqId = 33, tag = 18, index = 0, blockOffset = 0, rw = false, expectedData = Some("ca065d4eea469633ab2fd69681debb57")), // Critical core can evict any other core
    Stall(250), // Wait for some time before unset the core as non-critical
    PerformSchedulerOperation(addr = 1, rw = false),
    ExpectFinishedRejectedResponse(coreId = 2, reqId = 31, expectedData = "40a13302ac635051b398eb9a4cec416c"),
    ExpectFinishedRejectedResponse(coreId = 0, reqId = 32, expectedData = "d1f8fbeb63d88f19a2af35a0cd00f5ef"),
    CacheRequest(coreId = 3, reqId = 34, tag = 8, index = 4, blockOffset = 0, rw = false, expectedData = Some("bf7ecefbef86e816b49f6740df6d0069")),
    CacheRequest(coreId = 3, reqId = 35, tag = 0, index = 5, blockOffset = 2, rw = false, expectedData = Some("b7e1903f47db2c8efa81039b23a5f64e")),
    CacheRequest(coreId = 3, reqId = 36, tag = 1, index = 3, blockOffset = 1, rw = false, expectedData = Some("30464e5bf598385749afdfcfba3bae98")), // (HIT)
    CacheRequest(coreId = 0, reqId = 37, tag = 17, index = 0, blockOffset = 0, rw = false, expectedData = Some("d1f8fbeb63d88f19a2af35a0cd00f5ef")),
    CacheRequest(coreId = 2, reqId = 38, tag = 0, index = 4, blockOffset = 2, rw = false, expectedData = Some("314a8f9cd7e40cb26f19de83a481b2dc")), // (HIT)
  )

  // Test actions for a stress test
  val testActions3: Array[TestAction] = Array(
    // Test sequential reads from different memory regions
    CacheRequest(coreId = 0, reqId = 0, tag = 0, index = 0, blockOffset = 0, rw = false, expectedData = Some("cafebabebabecafedeadbeefbeefdead")), // Words 0-3: beefdead, deadbeef, babecafe, cafebabe
    CacheRequest(coreId = 1, reqId = 1, tag = 0, index = 0, blockOffset = 1, rw = false, expectedData = Some("deadbabebabedeadfeedfacefacefeed")), // Words 4-7: facefeed, feedface, babedead, deadbabe
    CacheRequest(coreId = 2, reqId = 2, tag = 0, index = 0, blockOffset = 2, rw = false, expectedData = Some("fee1deaddeadfee1c00010ff10ffc000")), // Words 8-11: 10ffc000, c00010ff, deadfee1, fee1dead
    CacheRequest(coreId = 3, reqId = 3, tag = 0, index = 0, blockOffset = 3, rw = false, expectedData = Some("dead10cc10ccdeadd00d2bad2badd00d")), // Words 12-15: 2badd00d, d00d2bad, 10ccdead, dead10cc
    // Test reads from mid-range addresses (around word 1000)
    CacheRequest(coreId = 0, reqId = 4, tag = 1, index = 0, blockOffset = 0, rw = false, expectedData = Some("bbef1226751129196ede4c8a9dc4fbd4")), // Words 1000-1003 region
    CacheRequest(coreId = 1, reqId = 5, tag = 1, index = 0, blockOffset = 1, rw = false, expectedData = Some("f8ab4690dd99254eb0eeda4a06a360bc")), // Words 1004-1007 region
    // Test reads from higher addresses (around word 5000)
    CacheRequest(coreId = 2, reqId = 6, tag = 5, index = 0, blockOffset = 0, rw = false, expectedData = Some("1172becdb246ca947b61e7da07672879")),
    CacheRequest(coreId = 3, reqId = 7, tag = 5, index = 0, blockOffset = 1, rw = false, expectedData = Some("cc5fd511c237a27e2451003dbc4d8025")),
    // Test interleaved reads and writes
    CacheRequest(coreId = 0, reqId = 8, tag = 0, index = 1, blockOffset = 0, rw = false, expectedData = Some("bbadbeefbeefbbade0ddf00dbadc0ffe")), // Words 16-19
    CacheRequest(coreId = 1, reqId = 9, tag = 0, index = 1, blockOffset = 1, rw = true, wData = Some("hdeadcafe00000000"), byteEn = Some("b0000000011110000")), // Partial write
    Stall(5),
    //    CacheRequest(coreId = 2, reqId = 10, tag = 0, index = 1, blockOffset = 1, rw = false, expectedData = Some("0d15ea5eea5e0d15deadcafed00dcafe")), // Read back modified data
    // Test cache conflicts and evictions with different tags, same index
    CacheRequest(coreId = 3, reqId = 11, tag = 2, index = 1, blockOffset = 0, rw = false, expectedData = Some("5094fd4c8f779c01498c95738c2435e3")), // Force different tag, same index
    CacheRequest(coreId = 0, reqId = 12, tag = 3, index = 1, blockOffset = 0, rw = false, expectedData = Some("30e2a17ee045d14ab58defac3308495e")), // Another different tag, same index
    CacheRequest(coreId = 1, reqId = 13, tag = 4, index = 1, blockOffset = 0, rw = false, expectedData = Some("8c6cab16656b3baa17613c7518180499")), // Yet another different tag
    // Test boundary conditions - end of memory regions
    CacheRequest(coreId = 2, reqId = 14, tag = 10, index = 127, blockOffset = 0, rw = false, expectedData = Some("817b131959fd217505137e68dc457d74")), // High index
    CacheRequest(coreId = 3, reqId = 15, tag = 10, index = 127, blockOffset = 3, rw = false, expectedData = Some("073645cb83479e5c4002236602d01f20")), // High index, high block offset
    // Test mixed access patterns - read, write, read same location
    CacheRequest(coreId = 0, reqId = 16, tag = 0, index = 2, blockOffset = 0, rw = false, expectedData = Some("beefcacecacebeeffeedfacefacefeed")), // Words 32-35
    CacheRequest(coreId = 1, reqId = 17, tag = 0, index = 2, blockOffset = 0, rw = true, wData = Some("hcafebabe000000000000000000000000"), byteEn = Some("b1111000000000000")),
    Stall(5),
    //    CacheRequest(coreId = 2, reqId = 18, tag = 0, index = 2, blockOffset = 0, rw = false, expectedData = Some("cafebabecacebeeffeedfacefacefeed")), // Read modified
    // Test all cores accessing same cache line simultaneously
    CacheRequest(coreId = 0, reqId = 19, tag = 0, index = 3, blockOffset = 0, rw = false, expectedData = Some("bd42f9c33e0b8a6ff71d24c989bc3e10")), // Words 36-39
    CacheRequest(coreId = 1, reqId = 20, tag = 0, index = 3, blockOffset = 1, rw = false, expectedData = Some("d239c4fa41e0bc97caf3b2186f83d1a5")), // Words 40-43
    CacheRequest(coreId = 2, reqId = 21, tag = 0, index = 3, blockOffset = 2, rw = false, expectedData = Some("56789abcef01234593cd78aba1e2f8d0")), // Words 44-47
    CacheRequest(coreId = 3, reqId = 22, tag = 0, index = 3, blockOffset = 3, rw = false, expectedData = Some("e1d0a5f737ed5f90cbfae10310293abc")), // Words 48-51
    // Test write-through behavior with byte enables
    CacheRequest(coreId = 0, reqId = 23, tag = 0, index = 4, blockOffset = 0, rw = true, wData = Some("hdeadbeef0000000000000000"), byteEn = Some("b011000000000")), // 2-byte write
    CacheRequest(coreId = 1, reqId = 24, tag = 0, index = 4, blockOffset = 0, rw = true, wData = Some("h000000000000000000000000cafebabe"), byteEn = Some("b0000000000001111")), // Different bytes
    Stall(5),
    //    CacheRequest(coreId = 2, reqId = 25, tag = 0, index = 4, blockOffset = 0, rw = false, expectedData = Some("8fb2741c9adbe137bc3f0e2acafebabe")), // Read combined write
    // Test large address space coverage
    CacheRequest(coreId = 3, reqId = 26, tag = 15, index = 63, blockOffset = 2, rw = false, expectedData = Some("1e7ddd1da8695c92691e32ca820d4be5")), // Mid-range tag and index
    CacheRequest(coreId = 0, reqId = 27, tag = 31, index = 31, blockOffset = 1, rw = false, expectedData = Some("db19b1635794a999575ef4b5da482462")), // Higher tag and index
    // Test rapid sequential access pattern
    CacheRequest(coreId = 1, reqId = 28, tag = 0, index = 10, blockOffset = 0, rw = false, expectedData = Some("feed123410badf00faceb00cc0ffeeee")), // Words around 640
    CacheRequest(coreId = 1, reqId = 29, tag = 0, index = 10, blockOffset = 1, rw = false, expectedData = Some("fa11babebabefacec01dbabea11face1")),
    CacheRequest(coreId = 1, reqId = 30, tag = 0, index = 10, blockOffset = 2, rw = false, expectedData = Some("ba5efacef005ba11d00dbabedeadfeed")),
    CacheRequest(coreId = 1, reqId = 31, tag = 0, index = 10, blockOffset = 3, rw = false, expectedData = Some("c0dec0debaddbabedead1337bad15ead")),
    // Test cross-core write conflicts
    CacheRequest(coreId = 2, reqId = 32, tag = 0, index = 20, blockOffset = 0, rw = true, wData = Some("haaaaaaaa0000000000000000"), byteEn = Some("b0000011000000000")),
    CacheRequest(coreId = 3, reqId = 33, tag = 0, index = 20, blockOffset = 0, rw = true, wData = Some("h00000000bbbbbbbb00000000"), byteEn = Some("b0000000011000000")),
    Stall(5),
    //    CacheRequest(coreId = 0, reqId = 34, tag = 0, index = 20, blockOffset = 0, rw = false, expectedData = Some("aaaaaaaabbbbbbbb0000000000000000")), // Read combined
    // Final stress test with high-frequency mixed operations
    CacheRequest(coreId = 1, reqId = 35, tag = 100, index = 100, blockOffset = 0, rw = false, expectedData = Some("7bdce363af7a7d087b11f4d10c7df004")), // High addresses
    CacheRequest(coreId = 2, reqId = 36, tag = 100, index = 100, blockOffset = 1, rw = true, wData = Some("hffffffff00000000"), byteEn = Some("b0000000011110000")),
    CacheRequest(coreId = 3, reqId = 37, tag = 100, index = 100, blockOffset = 2, rw = false, expectedData = Some("35d4c8b68346564ff6448cbc036d040b")),
    CacheRequest(coreId = 0, reqId = 38, tag = 100, index = 100, blockOffset = 3, rw = true, wData = Some("h000000000000000012345678"), byteEn = Some("b0000000000001111")),
    Stall(5),
    //    CacheRequest(coreId = 1, reqId = 39, tag = 100, index = 100, blockOffset = 3, rw = false, expectedData = Some("21c7660216f71ee6e3f0f7c2b1feb075")) // Read final state
  )

  // Test actions for miss-q and precedent events only
  val testActions4: Array[TestAction] = Array(
    PerformSchedulerOperation(2, rw = true, wData = Some(10)),
    PerformSchedulerOperation(1, rw = true, wData = Some(20)),
    CacheRequest(coreId = 0, reqId = 0, tag = 60, index = 74, blockOffset = 0, rw = false, expectedData = Some("ae51ffbe61691f90541ac32810690f94")), // MISS, way: 0
    CacheRequest(coreId = 3, reqId = 1, tag = 54, index = 74, blockOffset = 1, rw = false, expectedData = Some("cda5416b8ba4fa4e0d8244282c9b4387")), // MISS, way: 1
    CacheRequest(coreId = 2, reqId = 2, tag = 22, index = 74, blockOffset = 0, rw = false, expectedData = Some("3470e64103b3e3bbbba668cc45f7ce41")), // MISS, way: 2, miss-q event
    CacheRequest(coreId = 2, reqId = 3, tag = 60, index = 74, blockOffset = 1, rw = false, expectedData = Some("6ab05fb732eff46572bc10a708c9cc9e")), // HALF-MISS, way: 0
    CacheRequest(coreId = 2, reqId = 4, tag = 23, index = 74, blockOffset = 2, rw = false, expectedData = Some("609ad281fb126e36142a9ea1ec9c4494")), // MISS, way: 3, miss-q event
    CacheRequest(coreId = 2, reqId = 5, tag = 31, index = 74, blockOffset = 3, rw = false, expectedData = Some("d5a1b2aea383f9efe8454c936e1980ff")), // MISS, way: 4, mim event
    CacheRequest(coreId = 2, reqId = 6, tag = 44, index = 74, blockOffset = 1, rw = false, expectedData = Some("e93b3a7194d49fe2a0768f88d1761b37")), // MISS, way: 5,
    CacheRequest(coreId = 2, reqId = 7, tag = 60, index = 74, blockOffset = 3, rw = false, expectedData = Some("73b0278b6f36d7c8306be31e40beb1d6")), // HIT, way: 0, precedent event
    CacheRequest(coreId = 2, reqId = 8, tag = 22, index = 74, blockOffset = 0, rw = false, expectedData = Some("3470e64103b3e3bbbba668cc45f7ce41")), // HIT, way: 2,
    CacheRequest(coreId = 2, reqId = 9, tag = 47, index = 74, blockOffset = 1, rw = false, expectedData = Some("8bc9e5b16e398f2a0ad9d36219c0a11d")), // MISS, way: 6,
    CacheRequest(coreId = 2, reqId = 10, tag = 43, index = 74, blockOffset = 0, rw = false, expectedData = Some("bb8f359304bfc5ad924f17cfe23dd1e1")), // MISS, way: 7,
    CacheRequest(coreId = 2, reqId = 11, tag = 12, index = 74, blockOffset = 2, rw = false, expectedData = Some("27dc8a4a3c648e0f23e4cfb1207fb2ec")), // MISS, way: 0,
    CacheRequest(coreId = 2, reqId = 12, tag = 18, index = 74, blockOffset = 0, rw = false, expectedData = Some("c70d484dfb1674c75d14d293ce30ec7c")), // MISS, way: 1,
    Stall(50),
    CacheRequest(coreId = 3, reqId = 13, tag = 21, index = 74, blockOffset = 1, rw = false, expectedData = Some("efc887ce8779c45512b89afb7423b4d5")), // MISS, way: 2, contention event
    Stall(7), // Allow the miss request to enter the memory interface
    CacheRequest(coreId = 2, reqId = 14, tag = 39, index = 74, blockOffset = 3, rw = false, expectedData = Some("b638aaa4ef343eee6a4757cb65a2f78c")), // MISS, way: 3, mim event and contention event
    CacheRequest(coreId = 1, reqId = 15, tag = 8, index = 74, blockOffset = 0, rw = false, expectedData = Some("e83fb23952953ff164bdb8d5685d2bd3")), // MISS, way: 4, contention event
    CacheRequest(coreId = 2, reqId = 16, tag = 11, index = 74, blockOffset = 1, rw = false, expectedData = Some("42c953340ec5b7625bda6afc5422c666")), // MISS, way: 5, mim event and contention event, reach contention limit
    CacheRequest(coreId = 2, reqId = 17, tag = 57, index = 74, blockOffset = 2, rw = false, expectedData = Some("26f4756810b9c7b7fc87a234ac62fee6")), // MISS, way: 2,
    CacheRequest(coreId = 2, reqId = 18, tag = 43, index = 74, blockOffset = 1, rw = false, expectedData = Some("a9e7b6e5d8cc2100224ed5714cea356d")), // HIT, way: 7,
    CacheRequest(coreId = 2, reqId = 19, tag = 41, index = 74, blockOffset = 1, rw = false, expectedData = Some("8f2a3009871c1b8fb22ed80f63229d0f")), // MISS, way: 4, any non-critical way cannot evict any more lines in this set
    CacheRequest(coreId = 0, reqId = 20, tag = 2, index = 74, blockOffset = 1, rw = false, expectedData = Some("799773662e941d07b4340bdff6f72ec1"), rejected = true), // MISS, way: rejected
    CacheRequest(coreId = 3, reqId = 21, tag = 41, index = 73, blockOffset = 1, rw = false, expectedData = Some("43b459ab68e19d5012e7cb6afc7836ee")), // MISS, way: 0, allow some non-critical events to enter the miss queue
    CacheRequest(coreId = 3, reqId = 22, tag = 42, index = 73, blockOffset = 3, rw = false, expectedData = Some("763453aa42df7d92956636a260ab9d3b")), // MISS, way: 1
    CacheRequest(coreId = 2, reqId = 23, tag = 43, index = 73, blockOffset = 2, rw = false, expectedData = Some("8ce6a0927f254495140145d97c73d99b")), // MISS, way: 2, expect the critical request to enter the critical queue
    CacheRequest(coreId = 3, reqId = 24, tag = 43, index = 73, blockOffset = 0, rw = false, expectedData = Some("1166cabd710071a9c992e051ec16c43d")), // HALF-MISS, see if a non-critical request can be added as a half miss
    PerformSchedulerOperation(2, false),
    Stall(50),
    ExpectFinishedRejectedResponse(coreId = 0, reqId = 20, expectedData = "799773662e941d07b4340bdff6f72ec1")
  )

  // Test actions for a miss queue full of half misses
  val testActions5: Array[TestAction] = Array(
    CacheRequest(coreId = 0, reqId = 0, tag = 60, index = 57, blockOffset = 0, rw = false, expectedData = Some("02f6aa2d6c7f403950ad1a7166aeab2e")), // MISS, way: 0
    CacheRequest(coreId = 3, reqId = 1, tag = 60, index = 57, blockOffset = 1, rw = false, expectedData = Some("5ab22a1a86dd5005cf60419e7d51a925")), // HALF-MISS, way: 0
    CacheRequest(coreId = 2, reqId = 2, tag = 60, index = 57, blockOffset = 2, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000000000001101")), // HALF-MISS, way: 0
    CacheRequest(coreId = 2, reqId = 3, tag = 60, index = 57, blockOffset = 3, rw = true, wData = Some("hcafebabecafebabecafebabecafebabe"), byteEn = Some("b0000011100000000")), // HALF-MISS, way: 0
    CacheRequest(coreId = 1, reqId = 4, tag = 60, index = 57, blockOffset = 2, rw = false, expectedData = Some("1b9ec3d806fee9654ad7394c6f5414d3")), // HALF-MISS, way: 0
    CacheRequest(coreId = 1, reqId = 5, tag = 60, index = 57, blockOffset = 3, rw = false, expectedData = Some("5fba5cb28130e17479aa83d94b3062e7")), // HALF-MISS, way: 0
    CacheRequest(coreId = 2, reqId = 6, tag = 60, index = 57, blockOffset = 2, rw = false, expectedData = Some("1b9ec3d806fee9654ad7394cdead14ef")), // HIT, way: 0
    CacheRequest(coreId = 2, reqId = 7, tag = 60, index = 57, blockOffset = 3, rw = false, expectedData = Some("5fba5cb281febabe79aa83d94b3062e7")), // HIT, way: 0
  )

  // Test actions for wb events only
  val testActions6: Array[TestAction] = Array(
    PerformSchedulerOperation(addr = 2, rw = true, wData = Some(6)),
    PerformSchedulerOperation(addr = 1, rw = true, wData = Some(2)),
    CacheRequest(coreId = 2, reqId = 0, tag = 60, index = 74, blockOffset = 0, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000000000001111")), // MISS, way: 0
    CacheRequest(coreId = 3, reqId = 1, tag = 54, index = 74, blockOffset = 1, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000000011110000")), // MISS, way: 1
    CacheRequest(coreId = 1, reqId = 2, tag = 22, index = 74, blockOffset = 0, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000111100000000")), // MISS, way: 2
    CacheRequest(coreId = 3, reqId = 3, tag = 23, index = 74, blockOffset = 2, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b1111000000000000")), // MISS, way: 3
    CacheRequest(coreId = 2, reqId = 4, tag = 31, index = 74, blockOffset = 3, rw = true, wData = Some("hcafebabecafebabecafebabecafebabe"), byteEn = Some("b0000000000001111")), // MISS, way: 4
    CacheRequest(coreId = 2, reqId = 5, tag = 44, index = 74, blockOffset = 1, rw = true, wData = Some("hcafebabecafebabecafebabecafebabe"), byteEn = Some("b0000000011110000")), // MISS, way: 5
    CacheRequest(coreId = 2, reqId = 6, tag = 47, index = 74, blockOffset = 1, rw = true, wData = Some("hcafebabecafebabecafebabecafebabe"), byteEn = Some("b0000111100000000")), // MISS, way: 6,
    CacheRequest(coreId = 2, reqId = 7, tag = 43, index = 74, blockOffset = 0, rw = true, wData = Some("hcafebabecafebabecafebabecafebabe"), byteEn = Some("b1111000000000000")), // MISS, way: 7,
    CacheRequest(coreId = 0, reqId = 8, tag = 12, index = 74, blockOffset = 2, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000111100000000")), // MISS, way: 0, contention event, critical wb
    CacheRequest(coreId = 2, reqId = 9, tag = 18, index = 74, blockOffset = 0, rw = true, wData = Some("hcafebabecafebabecafebabecafebabe"), byteEn = Some("b0000000000001111")), // MISS, way: 1, wb event, non-critical wb
    CacheRequest(coreId = 3, reqId = 10, tag = 21, index = 74, blockOffset = 1, rw = false, expectedData = Some("efc887ce8779c45512b89afb7423b4d5")), // MISS, way: 2, contention event, critical wb
    CacheRequest(coreId = 2, reqId = 11, tag = 39, index = 74, blockOffset = 3, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b1111000000000000")), // MISS, way: 3, wb event, non-critical wb
    CacheRequest(coreId = 0, reqId = 12, tag = 8, index = 74, blockOffset = 0, rw = false, expectedData = Some("e83fb23952953ff164bdb8d5685d2bd3")), // MISS, way: 4, eviction event, critical-wb
    CacheRequest(coreId = 2, reqId = 13, tag = 43, index = 74, blockOffset = 1, rw = false, expectedData = Some("a9e7b6e5d8cc2100224ed5714cea356d")), // HIT, way: 7,
    CacheRequest(coreId = 2, reqId = 14, tag = 11, index = 74, blockOffset = 1, rw = false, expectedData = Some("42c953340ec5b7625bda6afc5422c666")), // MISS, way: 5, eviction event, critical-wb
    CacheRequest(coreId = 0, reqId = 15, tag = 2, index = 74, blockOffset = 1, rw = false, expectedData = Some("799773662e941d07b4340bdff6f72ec1")), // MISS, way: 6, eviction event, reach contention limit for core 2, critical-wb
    CacheRequest(coreId = 1, reqId = 16, tag = 57, index = 74, blockOffset = 2, rw = false, expectedData = Some("26f4756810b9c7b7fc87a234ac62fee6")), // MISS, way: 0, wb event, non-critical wb, reach contention for core 1
    CacheRequest(coreId = 1, reqId = 17, tag = 41, index = 74, blockOffset = 1, rw = false, expectedData = Some("8f2a3009871c1b8fb22ed80f63229d0f")), // MISS, way: 2
    CacheRequest(coreId = 1, reqId = 18, tag = 49, index = 74, blockOffset = 0, rw = false, expectedData = Some("1254107d21d6cebe035d8eb34c9a7eb5")), // MISS, way: 4
    CacheRequest(coreId = 1, reqId = 19, tag = 19, index = 74, blockOffset = 0, rw = false, expectedData = Some("7ee704ff89853f610b37152125de93af")), // MISS, way: 6
    CacheRequest(coreId = 2, reqId = 20, tag = 15, index = 74, blockOffset = 0, rw = false, expectedData = Some("7296c2863afadb5b23832ee2be161f15")), // MISS, way: 1, cause critical-wb
    CacheRequest(coreId = 3, reqId = 21, tag = 54, index = 74, blockOffset = 1, rw = true, wData = Some("hcafebabecafebabecafebabecafebabe"), byteEn = Some("b1111000000000000"), rejected = true), // MISS, way: rejected
    // Fill up another set with non-critical lines
    CacheRequest(coreId = 3, reqId = 22, tag = 60, index = 73, blockOffset = 0, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000000000001111")), // MISS, way: 0
    CacheRequest(coreId = 3, reqId = 23, tag = 54, index = 73, blockOffset = 1, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000000011110000")), // MISS, way: 1
    CacheRequest(coreId = 0, reqId = 24, tag = 22, index = 73, blockOffset = 0, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000111100000000")), // MISS, way: 2
    CacheRequest(coreId = 0, reqId = 25, tag = 23, index = 73, blockOffset = 2, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b1111000000000000")), // MISS, way: 3
    CacheRequest(coreId = 3, reqId = 26, tag = 31, index = 73, blockOffset = 3, rw = true, wData = Some("hcafebabecafebabecafebabecafebabe"), byteEn = Some("b0000000000001111")), // MISS, way: 4
    CacheRequest(coreId = 3, reqId = 27, tag = 44, index = 73, blockOffset = 1, rw = true, wData = Some("hcafebabecafebabecafebabecafebabe"), byteEn = Some("b0000000011110000")), // MISS, way: 5
    CacheRequest(coreId = 0, reqId = 28, tag = 47, index = 73, blockOffset = 1, rw = true, wData = Some("hcafebabecafebabecafebabecafebabe"), byteEn = Some("b0000111100000000")), // MISS, way: 6,
    Stall(200),
    CacheRequest(coreId = 0, reqId = 29, tag = 43, index = 73, blockOffset = 0, rw = true, wData = Some("hcafebabecafebabecafebabecafebabe"), byteEn = Some("b1111000000000000")), // MISS, way: 7,
    CacheRequest(coreId = 3, reqId = 30, tag = 12, index = 73, blockOffset = 2, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000111100000000")), // MISS, way: 0, evict non-critical line, cause non-critical wb
    CacheRequest(coreId = 2, reqId = 30, tag = 12, index = 74, blockOffset = 2, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000111100000000")), // MISS, way: 0, evict critical line, cause critical wb
    Stall(200),
    PerformSchedulerOperation(addr = 2, rw = false),
    ExpectFinishedRejectedResponse(coreId = 3, reqId = 21, expectedData = "cda5416b8ba4fa4edeadbeef2c9b4387")
  )

  // Test action array for timeout policy
  val testActions7: Array[TestAction] = Array(
    PerformSchedulerOperation(addr = 1, rw = true, wData = Some(6)), // Since we have 128 sets, the timer is equivalent to 128 * 4 = 512
    PerformSchedulerOperation(addr = 3, rw = true, wData = Some(5)), // Since we have 128 sets, the timer is equivalent to 128 * 3 = 384
    CacheRequest(coreId = 3, reqId = 0, tag = 0, index = 11, blockOffset = 1, rw = false, expectedData = Some("badf00d1deadd00ddeadf00de00dbabe")), // Bring new line into the cache (put in way: 0, idx: 11)
    CacheRequest(coreId = 3, reqId = 1, tag = 1, index = 11, blockOffset = 0, rw = false, expectedData = Some("6d5234bd430b687268b113258e2b13c3")), // Bring new line into the cache (put in way: 1, idx: 11)
    CacheRequest(coreId = 3, reqId = 2, tag = 2, index = 11, blockOffset = 0, rw = false, expectedData = Some("0d88a68af1d314bc0b7f893b5f07e697")), // Bring new line into the cache (put in way: 2, idx: 11)
    CacheRequest(coreId = 3, reqId = 3, tag = 3, index = 11, blockOffset = 2, rw = false, expectedData = Some("09f60e3ef165ed5677c24d7cea372dff")), // Bring new line into the cache (put in way: 3, idx: 11)
    CacheRequest(coreId = 3, reqId = 4, tag = 4, index = 11, blockOffset = 1, rw = false, expectedData = Some("8e29b9e854ad844693d8a0f8b5454aa9")), // Bring new line into the cache (put in way: 4, idx: 11)
    CacheRequest(coreId = 1, reqId = 5, tag = 5, index = 11, blockOffset = 1, rw = false, expectedData = Some("367a3411a5af83df7dea778f062cd679")), // Bring new line into the cache (put in way: 5, idx: 11),
    CacheRequest(coreId = 1, reqId = 6, tag = 6, index = 11, blockOffset = 0, rw = false, expectedData = Some("2cfadaa323bbabfda24bb33e1ec206ce")), // Bring new line into the cache (put in way: 6, idx: 11),
    CacheRequest(coreId = 1, reqId = 7, tag = 7, index = 11, blockOffset = 1, rw = false, expectedData = Some("d2a307d0c1ac087497ab025b37b649c9")), // Bring new line into the cache (put in way: 7, idx: 11),
    CacheRequest(coreId = 1, reqId = 8, tag = 0, index = 23, blockOffset = 1, rw = false, expectedData = Some("1aaa2c0b233399cb31feac1bc7441ed4")), // Bring new line into the cache (put in way: 0, idx: 23)
    CacheRequest(coreId = 3, reqId = 9, tag = 1, index = 23, blockOffset = 0, rw = false, expectedData = Some("e11ead1d9e8d34255108cc71184e2b66")), // Bring new line into the cache (put in way: 1, idx: 23)
    CacheRequest(coreId = 3, reqId = 10, tag = 2, index = 23, blockOffset = 0, rw = false, expectedData = Some("5091641e94d2f6dc4746b45ddd688925")), // Bring new line into the cache (put in way: 2, idx: 23)
    CacheRequest(coreId = 3, reqId = 11, tag = 3, index = 23, blockOffset = 2, rw = false, expectedData = Some("d2cc937fdd555e6504b3a2eb53fa8d0f")), // Bring new line into the cache (put in way: 3, idx: 23)
    CacheRequest(coreId = 3, reqId = 12, tag = 4, index = 23, blockOffset = 1, rw = false, expectedData = Some("97b9fcd6d0667e10a07a70ac396b6a6e")), // Bring new line into the cache (put in way: 4, idx: 23)
    CacheRequest(coreId = 1, reqId = 13, tag = 5, index = 23, blockOffset = 1, rw = false, expectedData = Some("f82be0daee1c46e6146420ea53182c39")), // Bring new line into the cache (put in way: 5, idx: 23),
    CacheRequest(coreId = 1, reqId = 14, tag = 6, index = 23, blockOffset = 0, rw = false, expectedData = Some("bc53babc02c04d57c67acbb6005691ca")), // Bring new line into the cache (put in way: 6, idx: 23),
    CacheRequest(coreId = 3, reqId = 15, tag = 7, index = 23, blockOffset = 1, rw = false, expectedData = Some("77ba56ed258f3b30282f13b6c3d366bf")), // Bring new line into the cache (put in way: 7, idx: 23),
    CacheRequest(coreId = 2, reqId = 16, tag = 8, index = 23, blockOffset = 0, rw = false, expectedData = Some("be3f4422beec45cbedee67559c392dcd"), rejected = true), // Try to evict line 0 idx 23, get rejected
    CacheRequest(coreId = 0, reqId = 17, tag = 9, index = 23, blockOffset = 0, rw = false, expectedData = Some("54e147d27169a16cc978d79543c85c9c"), rejected = true), // Try to evict line 1 idx 23, get rejected too
    CacheRequest(coreId = 3, reqId = 18, tag = 1, index = 23, blockOffset = 2, rw = false, expectedData = Some("82a59600a82bea69be5a3ea3cb0497e2")), // HIT, way: 1, refresh timer for line: 2 idx 23
    CacheRequest(coreId = 1, reqId = 19, tag = 2, index = 23, blockOffset = 3, rw = false, expectedData = Some("242af81cdc1d21f9c5e3b2873e607af7")), // HIT, way: 2, refresh timer for line: 3 idx 23
    CacheRequest(coreId = 0, reqId = 20, tag = 8, index = 11, blockOffset = 0, rw = false, expectedData = Some("62835ec7c17a39e585c08d4880f921f9"), rejected = true), // Try to evict line 0 idx 11, get rejected
    ExpectFinishedRejectedResponse(coreId = 0, reqId = 20, expectedData = "62835ec7c17a39e585c08d4880f921f9"),
    ExpectFinishedRejectedResponse(coreId = 2, reqId = 16, expectedData = "be3f4422beec45cbedee67559c392dcd"),
    ExpectFinishedRejectedResponse(coreId = 0, reqId = 17, expectedData = "54e147d27169a16cc978d79543c85c9c"),
    Stall(500), // Wait until the lines had timed out
    CacheRequest(coreId = 0, reqId = 21, tag = 10, index = 23, blockOffset = 2, rw = false, expectedData = Some("f9bfe1b8c3c9caad333403b8dbbd4e8c")), // Bring new line into the cache (put in way: 4, idx: 23)
    CacheRequest(coreId = 2, reqId = 22, tag = 11, index = 23, blockOffset = 2, rw = false, expectedData = Some("29426de6f805eb9864e32306480eeea4")), // Bring new line into the cache (put in way: 0, idx: 23)
    CacheRequest(coreId = 2, reqId = 23, tag = 12, index = 23, blockOffset = 2, rw = false, expectedData = Some("db5f5acbd0845670a0fab4f7534923e3")), // Bring new line into the cache (put in way: 0, idx: 23)
    CacheRequest(coreId = 0, reqId = 24, tag = 9, index = 11, blockOffset = 2, rw = false, expectedData = Some("a0466a0d2b2095e75273964fb1722f88")), // Bring new line into the cache (put in way: 1, idx: 11)
    CacheRequest(coreId = 0, reqId = 25, tag = 10, index = 11, blockOffset = 2, rw = false, expectedData = Some("820c616bd75be7e222996b21aa8c0d5c")), // Bring new line into the cache (put in way: 2, idx: 11)
    CacheRequest(coreId = 1, reqId = 26, tag = 0, index = 23, blockOffset = 2, rw = false, expectedData = Some("257d72a73dd426f89ac8f442f232adf6")), // Bring new line into the cache (put in way: 5, idx: 23)
    CacheRequest(coreId = 2, reqId = 27, tag = 2, index = 23, blockOffset = 2, rw = false, expectedData = Some("126ae598ef7181f2667ae7074b6c8ab2")), // HIT
  )

  // Test action array for contention, precedent, mim and wb events
  val testActions8: Array[TestAction] = Array(
    PerformSchedulerOperation(2, true, Some(5)),
    PerformSchedulerOperation(1, true, Some(4)),
    CacheRequest(coreId = 0, reqId = 0, tag = 60, index = 74, blockOffset = 0, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000000011110000")), // MISS, way: 0
    CacheRequest(coreId = 3, reqId = 1, tag = 54, index = 74, blockOffset = 1, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000111100000000")), // MISS, way: 1
    CacheRequest(coreId = 2, reqId = 2, tag = 22, index = 74, blockOffset = 0, rw = false, expectedData = Some("3470e64103b3e3bbbba668cc45f7ce41")), // MISS, way: 2, 2 miss-q events
    CacheRequest(coreId = 1, reqId = 3, tag = 23, index = 74, blockOffset = 2, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000000011110000")), // MISS, way: 3, 2 miss-q events
    CacheRequest(coreId = 2, reqId = 4, tag = 31, index = 74, blockOffset = 3, rw = false, expectedData = Some("d5a1b2aea383f9efe8454c936e1980ff")), // MISS, way: 4, mim event
    CacheRequest(coreId = 2, reqId = 5, tag = 44, index = 74, blockOffset = 1, rw = false, expectedData = Some("e93b3a7194d49fe2a0768f88d1761b37")), // MISS, way: 5,
    CacheRequest(coreId = 2, reqId = 6, tag = 60, index = 74, blockOffset = 3, rw = false, expectedData = Some("73b0278b6f36d7c8306be31e40beb1d6")), // HIT, way: 0, precedent event
    CacheRequest(coreId = 1, reqId = 7, tag = 54, index = 74, blockOffset = 0, rw = false, expectedData = Some("df40715150c3cdeb41342d9956c84263")), // HIT, way: 0, precedent event
    CacheRequest(coreId = 1, reqId = 8, tag = 47, index = 74, blockOffset = 1, rw = false, expectedData = Some("8bc9e5b16e398f2a0ad9d36219c0a11d")), // MISS, way: 6,
    CacheRequest(coreId = 2, reqId = 9, tag = 43, index = 74, blockOffset = 0, rw = false, expectedData = Some("bb8f359304bfc5ad924f17cfe23dd1e1")), // MISS, way: 7,
    CacheRequest(coreId = 1, reqId = 10, tag = 12, index = 74, blockOffset = 2, rw = false, expectedData = Some("27dc8a4a3c648e0f23e4cfb1207fb2ec")), // MISS, way: 0, evict non-critical line, cause wb
    CacheRequest(coreId = 2, reqId = 11, tag = 18, index = 74, blockOffset = 0, rw = false, expectedData = Some("c70d484dfb1674c75d14d293ce30ec7c")), // MISS, way: 1, evict non-critical line, cause wb, 1 wb event
    CacheRequest(coreId = 2, reqId = 12, tag = 21, index = 74, blockOffset = 1, rw = false, expectedData = Some("efc887ce8779c45512b89afb7423b4d5")), // MISS, way: 2, 2 wb events, reach contention for core 2
    CacheRequest(coreId = 3, reqId = 13, tag = 39, index = 74, blockOffset = 3, rw = false, expectedData = Some("b638aaa4ef343eee6a4757cb65a2f78c")), // MISS, way: 3, contention event, critical wb
    CacheRequest(coreId = 1, reqId = 14, tag = 57, index = 74, blockOffset = 1, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000111100000000")), // MISS, way: 6, mim and eviction event, reach contention for core 1
    CacheRequest(coreId = 2, reqId = 15, tag = 41, index = 74, blockOffset = 1, rw = false, expectedData = Some("8f2a3009871c1b8fb22ed80f63229d0f")), // MISS, way: 3,
    CacheRequest(coreId = 0, reqId = 16, tag = 41, index = 72, blockOffset = 1, rw = false, expectedData = Some("abbd90af6dbb29366ec5bd141df45023")), // MISS, way: 0, not rejected since different index
    CacheRequest(coreId = 1, reqId = 17, tag = 41, index = 72, blockOffset = 0, rw = false, expectedData = Some("a64a45812e63b4001eafac68edee5dd6")), // HALF-MISS, way: 1
    CacheRequest(coreId = 2, reqId = 18, tag = 41, index = 72, blockOffset = 2, rw = false, expectedData = Some("b1a10cf29e0b684ae2dd8277b34d19f1")), // HALF-MISS, way: 2
    CacheRequest(coreId = 0, reqId = 19, tag = 8, index = 74, blockOffset = 0, rw = false, expectedData = Some("e83fb23952953ff164bdb8d5685d2bd3"), rejected = true), // MISS, way: rejected
    Stall(100),
    CacheRequest(coreId = 1, reqId = 20, tag = 57, index = 74, blockOffset = 2, rw = false, expectedData = Some("26f4756810b9c7b7fc87a234ac62fee6")), // HIT,
    Stall(300),
    PerformSchedulerOperation(2, false),
    CacheRequest(coreId = 1, reqId = 21, tag = 57, index = 74, blockOffset = 1, rw = false, expectedData = Some("60874082deadbeefa97a1896ff0b1476")),
    ExpectFinishedRejectedResponse(coreId = 0, reqId = 19, expectedData = "e83fb23952953ff164bdb8d5685d2bd3"),
  )

  val testActions9: Array[TestAction] = Array(
    CacheRequest(coreId = 0, reqId = 0, tag = 60, index = 51, blockOffset = 0, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000000000001111")), // MISS, way: 0
    CacheRequest(coreId = 1, reqId = 1, tag = 61, index = 51, blockOffset = 1, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000000011110000")), // MISS, way: 1
    CacheRequest(coreId = 2, reqId = 2, tag = 62, index = 51, blockOffset = 2, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000111100000000")), // MISS, way: 2
    CacheRequest(coreId = 3, reqId = 3, tag = 63, index = 51, blockOffset = 3, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b1111000000000000")), // MISS, way: 3
    CacheRequest(coreId = 0, reqId = 4, tag = 64, index = 51, blockOffset = 0, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000000000001111")), // MISS, way: 4
    CacheRequest(coreId = 1, reqId = 5, tag = 65, index = 51, blockOffset = 1, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000000011110000")), // MISS, way: 5
    CacheRequest(coreId = 2, reqId = 6, tag = 66, index = 51, blockOffset = 2, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000111100000000")), // MISS, way: 6
    CacheRequest(coreId = 3, reqId = 7, tag = 67, index = 51, blockOffset = 3, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b1111000000000000")), // MISS, way: 7
    Stall(200), // Wait until all the data is brought into the cache
    CacheRequest(coreId = 0, reqId = 8, tag = 68, index = 51, blockOffset = 0, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b1111000000000000")), // MISS, way: 0
    CacheRequest(coreId = 1, reqId = 9, tag = 60, index = 51, blockOffset = 0, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000000011110000")), // MISS, way: 1
    CacheRequest(coreId = 2, reqId = 10, tag = 60, index = 51, blockOffset = 0, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000111100000000")), // HALF-MISS, way: 1
    CacheRequest(coreId = 3, reqId = 11, tag = 60, index = 51, blockOffset = 0, rw = true, wData = Some("hcafebabecafebabecafebabecafebabe"), byteEn = Some("b1111000000000000")), // HALF-MISS, way: 1
    CacheRequest(coreId = 0, reqId = 12, tag = 61, index = 51, blockOffset = 1, rw = false, expectedData = Some("d577c0716387d335deadbeefa43048c8")), // MISS, way: 2
    CacheRequest(coreId = 1, reqId = 13, tag = 62, index = 51, blockOffset = 2, rw = false, expectedData = Some("04864642deadbeef864612cf4309b4a3")), // MISS, way: 3
    CacheRequest(coreId = 2, reqId = 14, tag = 63, index = 51, blockOffset = 3, rw = false, expectedData = Some("deadbeef708879704d1bd26832e52d44")), // MISS, way: 4
    CacheRequest(coreId = 3, reqId = 15, tag = 64, index = 51, blockOffset = 0, rw = false, expectedData = Some("7f7dc81bd798aa6eabd7c90fdeadbeef")), // MISS, way: 5
    CacheRequest(coreId = 0, reqId = 16, tag = 65, index = 51, blockOffset = 1, rw = false, expectedData = Some("0ad39f3c41219d7bdeadbeef5596fcfc")), // MISS, way: 6
    Stall(330),
    CacheRequest(coreId = 1, reqId = 17, tag = 66, index = 51, blockOffset = 2, rw = false, expectedData = Some("df8dbf84deadbeef5572df5157b7c176")), // MISS, way: 0 *
    CacheRequest(coreId = 2, reqId = 18, tag = 60, index = 51, blockOffset = 0, rw = false, expectedData = Some("cafebabedeadbeefdeadbeefdeadbeef")), // HIT, way: 1 *
    CacheRequest(coreId = 3, reqId = 19, tag = 61, index = 51, blockOffset = 1, rw = false, expectedData = Some("d577c0716387d335deadbeefa43048c8")), // HIT, way: 2
    CacheRequest(coreId = 0, reqId = 20, tag = 62, index = 51, blockOffset = 2, rw = false, expectedData = Some("04864642deadbeef864612cf4309b4a3")), // HIT, way: 3
    CacheRequest(coreId = 1, reqId = 21, tag = 63, index = 51, blockOffset = 3, rw = false, expectedData = Some("deadbeef708879704d1bd26832e52d44")), // HIT, way: 4
    CacheRequest(coreId = 2, reqId = 22, tag = 64, index = 51, blockOffset = 0, rw = false, expectedData = Some("7f7dc81bd798aa6eabd7c90fdeadbeef")), // HIT, way: 5
    CacheRequest(coreId = 3, reqId = 23, tag = 65, index = 51, blockOffset = 1, rw = false, expectedData = Some("0ad39f3c41219d7bdeadbeef5596fcfc")), // HIT, way: 6
    CacheRequest(coreId = 3, reqId = 24, tag = 67, index = 51, blockOffset = 3, rw = false, expectedData = Some("deadbeef3f17911b3db7e6a0a19f928d")), // HIT, way: 7
    CacheRequest(coreId = 0, reqId = 25, tag = 69, index = 51, blockOffset = 0, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000000011110000")), // MISS, way: 0
    CacheRequest(coreId = 1, reqId = 26, tag = 68, index = 51, blockOffset = 0, rw = false, expectedData = Some("deadbeef2d21428029c70c78c255cab6")), // MISS, way: 1 *
    CacheRequest(coreId = 2, reqId = 27, tag = 70, index = 51, blockOffset = 0, rw = true, wData = Some("hcafebabecafebabecafebabecafebabe"), byteEn = Some("b0000111100000000")), // MISS, way: 2
    CacheRequest(coreId = 0, reqId = 28, tag = 62, index = 51, blockOffset = 2, rw = false, expectedData = Some("04864642deadbeef864612cf4309b4a3")), // HIT, way: 3
    CacheRequest(coreId = 1, reqId = 29, tag = 63, index = 51, blockOffset = 3, rw = false, expectedData = Some("deadbeef708879704d1bd26832e52d44")), // HIT, way: 4
    CacheRequest(coreId = 2, reqId = 30, tag = 64, index = 51, blockOffset = 0, rw = false, expectedData = Some("7f7dc81bd798aa6eabd7c90fdeadbeef")), // HIT, way: 5
    CacheRequest(coreId = 3, reqId = 31, tag = 65, index = 51, blockOffset = 1, rw = false, expectedData = Some("0ad39f3c41219d7bdeadbeef5596fcfc")), // HIT, way: 6
    CacheRequest(coreId = 3, reqId = 32, tag = 67, index = 51, blockOffset = 3, rw = false, expectedData = Some("deadbeef3f17911b3db7e6a0a19f928d")), // HIT, way: 7
    Stall(120),
    CacheRequest(coreId = 0, reqId = 33, tag = 69, index = 51, blockOffset = 0, rw = false, expectedData = Some("e7378b7ddf2a00e0deadbeefadc1c69d")), // HIT, way: 0 *
    CacheRequest(coreId = 1, reqId = 34, tag = 68, index = 51, blockOffset = 0, rw = false, expectedData = Some("deadbeef2d21428029c70c78c255cab6")), // HIT, way: 1
    CacheRequest(coreId = 2, reqId = 35, tag = 71, index = 51, blockOffset = 0, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b0000000011110000")), // MISS, way: 2
    Stall(60),
    CacheRequest(coreId = 3, reqId = 36, tag = 70, index = 51, blockOffset = 0, rw = false, expectedData = Some("6503d903cafebabe92e1c1015ed1d320")), // MISS, way: 3 *
    CacheRequest(coreId = 0, reqId = 37, tag = 71, index = 51, blockOffset = 0, rw = false, expectedData = Some("e143e293836ac3e3deadbeef92c8bbbe")), // HIT, way: 2 *
    Stall(60),
    CacheRequest(coreId = 1, reqId = 38, tag = 69, index = 51, blockOffset = 1, rw = true, wData = Some("hdeadbeefdeadbeefdeadbeefdeadbeef"), byteEn = Some("b1111000000000000")), // HIT, way: 0
    CacheRequest(coreId = 2, reqId = 39, tag = 68, index = 51, blockOffset = 0, rw = false, expectedData = Some("deadbeef2d21428029c70c78c255cab6")), // HIT, way: 1
    CacheRequest(coreId = 3, reqId = 40, tag = 70, index = 51, blockOffset = 0, rw = false, expectedData = Some("6503d903cafebabe92e1c1015ed1d320")), // HIT, way: 3
    CacheRequest(coreId = 0, reqId = 41, tag = 63, index = 51, blockOffset = 3, rw = false, expectedData = Some("deadbeef708879704d1bd26832e52d44")), // HIT, way: 4
    CacheRequest(coreId = 1, reqId = 42, tag = 64, index = 51, blockOffset = 0, rw = false, expectedData = Some("7f7dc81bd798aa6eabd7c90fdeadbeef")), // HIT, way: 5
    CacheRequest(coreId = 2, reqId = 43, tag = 65, index = 51, blockOffset = 1, rw = false, expectedData = Some("0ad39f3c41219d7bdeadbeef5596fcfc")), // HIT, way: 6
    CacheRequest(coreId = 3, reqId = 44, tag = 67, index = 51, blockOffset = 3, rw = false, expectedData = Some("deadbeef3f17911b3db7e6a0a19f928d")), // HIT, way: 7
    CacheRequest(coreId = 0, reqId = 45, tag = 72, index = 51, blockOffset = 2, rw = false, expectedData = Some("a7dd374354aafffed70ed97ed77322b5")), // MISS, way: 0 *
    CacheRequest(coreId = 1, reqId = 46, tag = 69, index = 51, blockOffset = 1, rw = false, expectedData = Some("deadbeefbf622032026a1da9617116da")), // MISS, way: 1
    // TODO: Add a test case in here, for when a memory interface pops an entry from miss-Q and at the same time a half miss is in the last REP stage
  )
}

object CacheConfigs {
  val config64BitPlru = CacheConfiguration(
    sizeInBytes = 65536,
    nCores = 4,
    nWays = 8,
    addressWidth = 25,
    reqIdWidth = 16,
    bytesPerBlock = 64,
    bytesPerSubBlock = 16,
    repPolConfig = BitPlruConfiguration(),
    memBeatSize = 4,
    memBurstLen = 4,
    repSetFormat = None,
    memFile = Some("./hex/test_mem_32w.hex"),
    nHalfMissCmds = Some(6)
  )

  val config64TreePlru = CacheConfiguration(
    sizeInBytes = 65536,
    nCores = 4,
    nWays = 8,
    addressWidth = 25,
    reqIdWidth = 16,
    bytesPerBlock = 64,
    bytesPerSubBlock = 16,
    repPolConfig = TreePlruConfiguration(),
    memBeatSize = 4,
    memBurstLen = 4,
    repSetFormat = None,
    memFile = Some("./hex/test_mem_32w.hex")
  )

  val config64TimeOut = CacheConfiguration(
    sizeInBytes = 65536,
    nCores = 4,
    nWays = 8,
    addressWidth = 25,
    reqIdWidth = 16,
    bytesPerBlock = 64,
    bytesPerSubBlock = 16,
    repPolConfig = TimeoutConfiguration(BasePolicies.BIT_PLRU),
    memBeatSize = 4,
    memBurstLen = 4,
    repSetFormat = Some(new MruFormat),
    memFile = Some("./hex/test_mem_32w.hex")
  )

  val config64Cont = CacheConfiguration(
    sizeInBytes = 65536,
    nCores = 4,
    nWays = 8,
    addressWidth = 25,
    reqIdWidth = 16,
    bytesPerBlock = 64,
    bytesPerSubBlock = 16,
    repPolConfig = ContentionConfiguration(BasePolicies.BIT_PLRU),
    memBeatSize = 4,
    memBurstLen = 4,
    repSetFormat = Some(new MruFormat),
    memFile = Some("./hex/test_mem_32w.hex")
  )

  val config64ContWb = CacheConfiguration(
    sizeInBytes = 65536,
    nCores = 4,
    nWays = 8,
    addressWidth = 25,
    reqIdWidth = 16,
    bytesPerBlock = 64,
    bytesPerSubBlock = 16,
    repPolConfig = ContentionConfiguration(BasePolicies.BIT_PLRU, wb = true),
    memBeatSize = 4,
    memBurstLen = 4,
    repSetFormat = Some(new MruFormat),
    memFile = Some("./hex/test_mem_32w.hex")
  )

  val config64ContMimPrec = CacheConfiguration(
    sizeInBytes = 65536,
    nCores = 4,
    nWays = 8,
    addressWidth = 25,
    reqIdWidth = 16,
    bytesPerBlock = 64,
    bytesPerSubBlock = 16,
    repPolConfig = ContentionConfiguration(BasePolicies.BIT_PLRU, mim = true, precedent = true),
    memBeatSize = 4,
    memBurstLen = 4,
    repSetFormat = Some(new MruFormat),
    memFile = Some("./hex/test_mem_32w.hex")
  )

  val config64ContMimPrecWb = CacheConfiguration(
    sizeInBytes = 65536,
    nCores = 4,
    nWays = 8,
    addressWidth = 25,
    reqIdWidth = 16,
    bytesPerBlock = 64,
    bytesPerSubBlock = 16,
    repPolConfig = ContentionConfiguration(BasePolicies.BIT_PLRU, mim = true, precedent = true, wb = true),
    memBeatSize = 4,
    memBurstLen = 4,
    repSetFormat = Some(new MruFormat),
    memFile = Some("./hex/test_mem_32w.hex")
  )
}

object SharedPipelinedCacheTest {
  val PRINT_RESULTS = false

  val PRINT_INFO = false

  def generateDut(cacheConfig: CacheConfiguration): Dut = {
    val nSets = cacheConfig.sizeInBytes / (cacheConfig.nWays * cacheConfig.bytesPerBlock)

    val l2RepPolicy = generateReplacementPolicy(
      cacheConfig.repPolConfig,
      cacheConfig.nWays,
      nSets,
      cacheConfig.nCores,
      cacheConfig.repSetFormat
    )

    val indexWidth = log2Up(nSets)
    val blockOffsetWidth = log2Up(cacheConfig.bytesPerBlock / cacheConfig.bytesPerSubBlock)
    val byteOffsetWidth = log2Up(cacheConfig.bytesPerSubBlock)
    val tagWidth = cacheConfig.addressWidth - indexWidth - blockOffsetWidth - byteOffsetWidth

    val cacheGenFun = () => new SharedPipelinedCacheTestTop(
      sizeInBytes = cacheConfig.sizeInBytes,
      nWays = cacheConfig.nWays,
      nCores = cacheConfig.nCores,
      reqIdWidth = cacheConfig.reqIdWidth,
      addressWidth = cacheConfig.addressWidth,
      bytesPerBlock = cacheConfig.bytesPerBlock,
      bytesPerSubBlock = cacheConfig.bytesPerSubBlock,
      memBeatSize = cacheConfig.memBeatSize,
      memBurstLen = cacheConfig.memBurstLen,
      l2RepPolicyGen = l2RepPolicy,
      dataFile = cacheConfig.memFile,
      nHalfMissCmds = cacheConfig.nHalfMissCmds,
      printCacheInfo = PRINT_INFO
    )

    Dut(cacheGenFun, cacheConfig.nCores, tagWidth, indexWidth, blockOffsetWidth, byteOffsetWidth)
  }

  def generateReplacementPolicy(policyConfig: PolicyConfiguration, nWays: Int, nSets: Int, nCores: Int, repSetFormat: Option[BaseReplacementSetFormat]): () => SharedCacheReplacementPolicyType = {
    def buildPolicy(repSetFormat: Option[BaseReplacementSetFormat])(default: => SharedCacheReplacementPolicyType, withFormat: BaseReplacementSetFormat => SharedCacheReplacementPolicyType): SharedCacheReplacementPolicyType =
      repSetFormat.map(withFormat).getOrElse(default)

    val policy = policyConfig match {
      case BitPlruConfiguration() => () => buildPolicy(repSetFormat)(new BitPlruReplacementPolicy(nWays, nSets, nCores), fmt => new BitPlruReplacementPolicy(nWays, nSets, nCores, fmt))
      case TreePlruConfiguration() => () => buildPolicy(repSetFormat)(new TreePlruReplacementPolicy(nWays, nSets, nCores), fmt => new TreePlruReplacementPolicy(nWays, nSets, nCores, fmt))
      case ContentionConfiguration(base, mim, precedent, wb) => () => buildPolicy(repSetFormat)(new ContentionReplacementPolicy(nWays, nSets, nCores, base, mim, precedent, wb), fmt => new ContentionReplacementPolicy(nWays, nSets, nCores, base, mim, precedent, wb, repSetFormat = fmt))
      case TimeoutConfiguration(base) => () => buildPolicy(repSetFormat)(new TimeoutReplacementPolicy(nWays, nSets, nCores, base), fmt => new TimeoutReplacementPolicy(nWays, nSets, nCores, base, fmt))
      case _ => throw new Exception("Unexpected policy configuration")
    }

    policy
  }

  def defaultAssignments(dut: SharedPipelinedCacheTestTop, nCores: Int): Unit = {
    // Default inputs
    for (i <- 0 until nCores) {
      dut.io.requests.cores(i).req.reqId.valid.poke(false.B)
      dut.io.requests.cores(i).req.reqId.bits.poke(0.U)
      dut.io.requests.cores(i).req.addr.poke(0.U)
      dut.io.requests.cores(i).req.rw.poke(false.B)
      dut.io.requests.cores(i).req.wData.poke(0.U)
    }

    dut.io.scheduler.cmd.poke(SchedulerCmd.NULL)
    dut.io.scheduler.addr.poke(0.U)
    dut.io.scheduler.wData.poke(0.U)

    dut.clock.step(5)
  }

  def setCoreAsCritical(dut: SharedPipelinedCacheTestTop, coreID: Int, contentionLimit: Int): Unit = {
    dut.io.scheduler.cmd.poke(SchedulerCmd.WR)
    dut.io.scheduler.addr.poke(coreID.U)
    dut.io.scheduler.wData.poke(contentionLimit.U)

    dut.clock.step(1)

    // Reset the signals
    dut.io.scheduler.cmd.poke(SchedulerCmd.NULL)
    dut.io.scheduler.addr.poke(0.U)
    dut.io.scheduler.wData.poke(0.U)
  }

  def unsetCoreAsCritical(dut: SharedPipelinedCacheTestTop, coreID: Int): Unit = {
    dut.io.scheduler.cmd.poke(SchedulerCmd.RD)
    dut.io.scheduler.addr.poke(coreID.U)

    dut.clock.step(1)

    // Reset the signals
    dut.io.scheduler.cmd.poke(SchedulerCmd.NULL)
    dut.io.scheduler.addr.poke(0.U)
  }

  def expectCacheResponse(dut: SharedPipelinedCacheTestTop, coreId: Int, reqId: Int, expectedData: String): Unit = {
    dut.io.requests.cores(coreId).resp.reqId.valid.expect(true.B, s"Did not receive a response for request: $reqId.")
    dut.io.requests.cores(coreId).resp.reqId.bits.expect(reqId.U, s"Did not receive a response for request: $reqId.")
    dut.io.requests.cores(coreId).resp.rData.expect(expectedData.U, s"Did not receive correct data for a request: $reqId.")
  }

  def performTestActions[T <: TestAction](
                                           dut: SharedPipelinedCacheTestTop,
                                           nCores: Int,
                                           testActions: Array[T],
                                           indexWidth: Int,
                                           blockOffsetWidth: Int,
                                           byteOffsetWidth: Int,
                                           maxCCs: Int
                                         ): Unit = {
    val responses = mutable.Set[CacheResponse]()
    var previousRequestCore: Option[Int] = None
    var actionIdx = 0
    var currentCC = 0
    var stallCycle: Option[Int] = None
    var receivedResponseCnt = 0
    val expectedRespCnt = testActions.count {
      case CacheRequest(_, _, _, _, _, _, _, rejected, _, _, _) if !rejected => true
      case ExpectFinishedRejectedResponse(_, _, _) => true
      case _ => false
    }

    // TODO: Group all requests by core ID, and issue request for each core at a same time,
    //  do not issue more requests for the same core if did not receive a response,
    //  add dependency for PerformSchedulerOperation action so it only executes it after a response to some
    //  request has been received

    while (currentCC < maxCCs && receivedResponseCnt != expectedRespCnt) {

      // Need to unset the request signals for the previous request
      if (previousRequestCore.isDefined) {
        val coreIdx = previousRequestCore.get
        dut.io.requests.cores(coreIdx).req.reqId.valid.poke(false.B)
        dut.io.requests.cores(coreIdx).req.reqId.bits.poke(0.U)
        dut.io.requests.cores(coreIdx).req.addr.poke(0.U)
        dut.io.requests.cores(coreIdx).req.rw.poke(false.B)
        dut.io.requests.cores(coreIdx).req.wData.poke(0.U)
      }

      if (actionIdx < testActions.length) {
        if (stallCycle.isEmpty) { // If we are stalling, we do not perform any actions
          val action = testActions(actionIdx)

          action match {
            case CacheRequest(coreId, reqId, rw, tag, index, blockOffset, byteOffset, _, byteEn, wData, _) =>
              dut.io.requests.cores(coreId).req.reqId.valid.poke(true.B)
              if (dut.io.requests.cores(coreId).req.reqId.ready.peekBoolean()) {
                val addr = tag << (indexWidth + blockOffsetWidth + byteOffsetWidth) |
                  index << (blockOffsetWidth + byteOffsetWidth) |
                  blockOffset << byteOffsetWidth |
                  byteOffset

                if (PRINT_INFO) {
                  println(s"Issued request at CC $currentCC: " +
                    s"Core: $coreId, " +
                    s"ReqId: $reqId, " +
                    s"Addr: ${addr.toHexString}, " +
                    s"RW: $rw, " +
                    s"WData: ${wData.getOrElse("None")}, " +
                    s"ByteEn: ${byteEn.getOrElse((math.pow(2, dut.subBlockDataWidth / 8).toInt - 1).toBinaryString)}")
                }

                performCacheRequest(dut, coreId = coreId, reqId = reqId, addr = addr, rw = rw, wData = wData, byteEn = byteEn)
                previousRequestCore = Some(coreId)

                actionIdx += 1
              } else {
                previousRequestCore = None
              }
            case Stall(cycles) =>
              if (PRINT_INFO) {
                println(s"Waiting for $cycles cycles at CC $currentCC, before issuing next request.")
              }
              previousRequestCore = None
              stallCycle = Some(currentCC + cycles)
              actionIdx += 1
            case PerformSchedulerOperation(addr, rw, wData) =>
              val wDataVal = wData.getOrElse(0)
              if (PRINT_INFO) {
                println(s"Performing scheduler operation at CC $currentCC: ${if (rw) "RW" else "RD"}, for addr: $addr, with data: $wDataVal")
              }
              performSchedulerOperation(dut, addr, rw, wDataVal)
              previousRequestCore = None
              actionIdx += 1
            case ExpectFinishedRejectedResponse(_, _, _) =>
              previousRequestCore = None
              actionIdx += 1
            case t => throw new Exception(s"Received unexpected action type: ${t.getClass.getSimpleName}")
          }
        } else {
          if (stallCycle.get == currentCC) { // Once we reached the stall cycle, we can clear it
            stallCycle = None
          }
        }
      } else {
        previousRequestCore = None
      }

      val response = checkForResponse(dut, currentCC, nCores)

      if (response.isDefined) {
        responses.add(response.get)
        receivedResponseCnt += 1
      }

      dut.clock.step(1)
      currentCC += 1
    }

    if (PRINT_INFO) {
      println(s"Test ran for: $currentCC CCs")
      println(s"Expected number of responses: $expectedRespCnt")
    }

    if (PRINT_RESULTS) {
      println("")
      val sortedResponse = responses.toSeq.sortBy(req => req.receivedCC)
      println(s"Received ${sortedResponse.size} responses in total.")
      for (resp <- sortedResponse) {
        println(s"Response: CC: ${resp.receivedCC}, Core: ${resp.coreId}, ReqId: ${resp.reqId}, Data: ${resp.data}")
      }
    }

    // Assert the number of responses matches expected number of responses
    val requestsWithExpectedResponse = testActions.filter {
      case CacheRequest(_, _, rw, _, _, _, _, rejected, _, _, _) if !rejected && !rw => true
      case ExpectFinishedRejectedResponse(_, _, _) => true
      case _ => false
    }

    // Assert the received data
    for (req <- requestsWithExpectedResponse) {
      req match {
        case CacheRequest(coreId, reqId, _, _, _, _, _, _, _, _, expectedData) =>
          val response = responses.find(resp => resp.coreId == coreId && resp.reqId == reqId)

          assert(response.isDefined, s"Did not receive a response for request: $reqId from core: $coreId.")
          assert(response.get.data == expectedData.get, s"Received data: ${response.get.data} does not match expected data: ${expectedData.get} for request: $reqId from core: $coreId.")
        case ExpectFinishedRejectedResponse(coreId, reqId, expectedData) =>
          val response = responses.find(resp => resp.coreId == coreId && resp.reqId == reqId)

          assert(response.isDefined, s"Did not receive a response for a previously rejected request: $reqId from core: $coreId.")
          assert(response.get.data == expectedData, s"Received data: ${response.get.data} does not match expected data: $expectedData for rejected request: $reqId from core: $coreId.")
        case t => throw new Exception(s"Received action type other than ${CacheRequest.getClass.getSimpleName}: ${t.getClass.getSimpleName}")
      }
    }

    if (PRINT_RESULTS) {
      println("")
    }
  }

  def performSchedulerOperation(dut: SharedPipelinedCacheTestTop, addr: Int, rw: Boolean, wData: Int): Unit = {
    if (rw) {
      dut.io.scheduler.cmd.poke(SchedulerCmd.WR)
      dut.io.scheduler.wData.poke(wData.U)
    } else {
      dut.io.scheduler.cmd.poke(SchedulerCmd.RD)
    }

    dut.io.scheduler.addr.poke(addr.U)

    dut.clock.step(1)

    // Reset the signals
    dut.io.scheduler.cmd.poke(SchedulerCmd.NULL)
    dut.io.scheduler.addr.poke(0.U)
    dut.io.scheduler.wData.poke(0.U)
  }

  def checkForResponse(dut: SharedPipelinedCacheTestTop, clockCycle: Int, nCores: Int): Option[CacheResponse] = {
    for (i <- 0 until nCores) {
      if (dut.io.requests.cores(i).resp.reqId.valid.peek().litToBoolean) {
        val reqId = dut.io.requests.cores(i).resp.reqId.bits.peek().litValue.toInt
        val data = dut.io.requests.cores(i).resp.rData.peek().litValue.toString(16).reverse.padTo(dut.subBlockDataWidth / 4, '0').reverse

        return Some(CacheResponse(
          receivedCC = clockCycle,
          coreId = i,
          reqId = reqId,
          data = data
        ))
      }
    }

    None
  }

  def performCacheRequest(dut: SharedPipelinedCacheTestTop, coreId: Int, reqId: Int, addr: Int, rw: Boolean, wData: Option[String] = None, byteEn: Option[String] = None): Unit = {
    // Expect the cache to be ready to accept a request
    dut.io.requests.cores(coreId).req.reqId.ready.expect(true.B)

    // Make request on behalf of the second core
    dut.io.requests.cores(coreId).req.reqId.valid.poke(true.B)
    dut.io.requests.cores(coreId).req.reqId.bits.poke(reqId.U)
    dut.io.requests.cores(coreId).req.addr.poke(addr.U)
    dut.io.requests.cores(coreId).req.rw.poke(rw.B)

    val wDataValue = wData match {
      case Some(data) => data.U
      case None => 0.U
    }

    val byteEnValue = byteEn match {
      case Some(data) => data.U
      case None => (math.pow(2, dut.subBlockDataWidth / 8).toInt - 1).U
    }

    dut.io.requests.cores(coreId).req.wData.poke(wDataValue)
    dut.io.requests.cores(coreId).req.byteEn.poke(byteEnValue)
  }
}
