package caches.hardware.pipelined.cache

import caches.hardware.pipelined.SharedPipelinedCacheTestTop
import chisel3._
import chiseltest._

/** Helpers for end-to-end tests that issue single requests to the cache and wait for their responses. */
object SharedPipelinedCacheRequestHelpers {
  def lineAddr(cache: Dut, tag: Int, index: Int): Int = {
    (tag << (cache.indexWidth + cache.blockOffsetWidth + cache.byteOffsetWidth)) |
      (index << (cache.blockOffsetWidth + cache.byteOffsetWidth))
  }

  /** Issues a request on one cycle and waits until the cache has accepted it. */
  def issue(dut: SharedPipelinedCacheTestTop, coreId: Int, reqId: Int, addr: Int, write: Option[BigInt] = None): Unit = {
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
  def waitForResponse(dut: SharedPipelinedCacheTestTop, coreId: Int, reqId: Int, maxCCs: Int = 2000): BigInt = {
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

  def assertNoResponse(dut: SharedPipelinedCacheTestTop, coreId: Int, cycles: Int): Unit = {
    for (_ <- 0 until cycles) {
      assert(!dut.io.requests.cores(coreId).resp.reqId.valid.peekBoolean(), s"Core $coreId received a response although its request should be rejected.")
      dut.clock.step()
    }
  }

}
