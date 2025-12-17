package caches.hardware.pipelined

import chisel.lib.fifo.RegFifo
import chisel3._
import chisel3.util._

class RejectionQueueEntry(nCores: Int, addrWidth: Int, dataWidth: Int, reqIdWidth: Int) extends Bundle {
  val coreId = Input(UInt(log2Up(nCores).W))
  val reqId = Input(UInt(reqIdWidth.W))
  val addr = Input(UInt(addrWidth.W))
  val rw = Input(Bool()) // 0 - Read, 1 - Write
  val byteEn = Input(UInt((dataWidth / 8).W))
  val wData = Input(UInt(dataWidth.W))
}

/**
 * Queue for requests rejected by the L2 replacement policy.
 *
 * @param nCores
 * @param addrWidth
 * @param dataWidth
 * @param reqIdWidth
 * @param depth
 */
class RejectionQueue(nCores: Int, addrWidth: Int, dataWidth: Int, reqIdWidth: Int, depth: Int) extends Module {
  val io = IO(new Bundle {
    val push = Input(Bool())
    val pushEntry = new RejectionQueueEntry(nCores, addrWidth, dataWidth, reqIdWidth)
    val full = Output(Bool())
    val popEntry = Flipped(new CacheRequestIO(addrWidth, dataWidth, reqIdWidth))
    val popCoreId = Output(UInt(log2Up(nCores).W))
  })

  val coreIdWidth = log2Up(nCores)
  val cmdLen = coreIdWidth + reqIdWidth + addrWidth + 1 + (dataWidth / 8) + dataWidth

  val rejectQueue = Module(new RegFifo(UInt(cmdLen.W), depth))
  val pushData = Cat(io.pushEntry.coreId, io.pushEntry.reqId, io.pushEntry.addr, io.pushEntry.rw, io.pushEntry.byteEn, io.pushEntry.wData)

  rejectQueue.io.enq.valid := io.push
  rejectQueue.io.enq.bits := pushData
  rejectQueue.io.deq.ready := io.popEntry.reqId.ready
  val popData = rejectQueue.io.deq.bits

  io.popEntry.wData := popData(dataWidth - 1, 0)
  io.popEntry.byteEn := popData((dataWidth / 8) - 1 + dataWidth, dataWidth)
  io.popEntry.rw := popData((dataWidth / 8) + dataWidth)
  io.popEntry.addr := popData(addrWidth - 1 + (dataWidth / 8) + dataWidth + 1, (dataWidth / 8) + dataWidth + 1)
  io.popEntry.reqId.bits := popData(reqIdWidth - 1 + addrWidth + (dataWidth / 8) + dataWidth + 1, addrWidth + (dataWidth / 8) + dataWidth + 1)
  io.popCoreId := popData(coreIdWidth - 1 + reqIdWidth + addrWidth + (dataWidth / 8) + dataWidth + 1, reqIdWidth + addrWidth + (dataWidth / 8) + dataWidth + 1)

  io.full := !rejectQueue.io.enq.ready
  io.popEntry.reqId.valid := rejectQueue.io.deq.valid
}
