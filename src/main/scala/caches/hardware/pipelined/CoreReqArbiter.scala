package caches.hardware.pipelined

import caches.hardware.util.DecoupledMux
import chisel3._
import chisel3.util._

/**
 * Core to cache request arbiter. The arbiter always prioritizes the first request entry to handshake with.
 */
class CoreReqArbiter(nCores: Int, addrWidth: Int, dataWidth: Int, reqIdWidth: Int) extends Module {
  val io = IO(new Bundle {
    val req1 = new CacheRequestIO(addrWidth, dataWidth, reqIdWidth)
    val req1CoreID = Input(UInt(log2Up(nCores).W))
    val req2 = new CacheRequestIO(addrWidth, dataWidth, reqIdWidth)
    val req2CoreID = Input(UInt(log2Up(nCores).W))
    val out = Flipped(new CacheRequestIO(addrWidth, dataWidth, reqIdWidth))
    val outCoreID = Output(UInt(log2Up(nCores).W))
  })

  val sel = io.req1.reqId.valid
  val mux = DecoupledMux(io.req2.reqId, io.req1.reqId, sel = sel)

  io.out.reqId <> mux
  io.out.addr := Mux(sel, io.req1.addr, io.req2.addr)
  io.out.rw := Mux(sel, io.req1.rw, io.req2.rw)
  io.out.byteEn := Mux(sel, io.req1.byteEn, io.req2.byteEn)
  io.out.wData := Mux(sel, io.req1.wData, io.req2.wData)
  io.outCoreID := Mux(sel, io.req1CoreID, io.req2CoreID)
}
