package caches.hardware.pipelined

import caches.hardware.util.RegFifoWithStatus
import chisel.lib.fifo._
import chisel3._

class WbFifoEntryIO(tagWidth: Int, indexWidth: Int, blockWidth: Int) extends Bundle {
  val isCrit = Input(Bool())
  val tag = Input(UInt(tagWidth.W))
  val index = Input(UInt(indexWidth.W))
  val wbData = Input(UInt(blockWidth.W))
}

class WbFifoPushIO(tagWidth: Int, indexWidth: Int, blockWidth: Int) extends Bundle() {
  val push = Input(Bool())
  val pushEntry = new WbFifoEntryIO(tagWidth, indexWidth, blockWidth)
  val full = Output(Bool())
}

class WbFifoInfoIO(queueDepth: Int) extends Bundle() {
  val wbQueueCritReqs = Output(Vec(queueDepth, Bool()))
  val wbQueueValidReqs = Output(Vec(queueDepth, Bool()))
}

class WbFifoPopIO(tagWidth: Int, indexWidth: Int, blockWidth: Int) extends Bundle() {
  val pop = Input(Bool())
  val popEntry = Flipped(new WbFifoEntryIO(tagWidth, indexWidth, blockWidth))
  val empty = Output(Bool())
}

class WbQueueIO(tagWidth: Int, indexWidth: Int, blockWidth: Int, queueDepth: Int) extends Bundle {
  val push = new WbFifoPushIO(tagWidth, indexWidth, blockWidth)
  val pop = new WbFifoPopIO(tagWidth, indexWidth, blockWidth)
  val wbInfo = new WbFifoInfoIO(queueDepth)
  val isFirstInQCrit = Output(Bool())
}

class WbFifoIO(tagWidth: Int, indexWidth: Int, blockWidth: Int, queueDepth: Int) extends Bundle {
  val pushCrit = Input(Bool())
  val popQSel = Input(Bool())
  val push = new WbFifoPushIO(tagWidth, indexWidth, blockWidth)
  val pop = new WbFifoPopIO(tagWidth, indexWidth, blockWidth)
  val wbInfo = new WbFifoInfoIO(queueDepth)
  val critEmpty = Output(Bool())
  val nonCritEmpty = Output(Bool())
}

class WbQueue(queueDepth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int) extends Module {
  val io = IO(new WbQueueIO(tagWidth, indexWidth, blockWidth, queueDepth))

  // If need be, the data queue can be turned in to a mem based fifo
  // TODO: Switch wb data fifo from register based to memory based fifo
  val wbDataFifo = Module(new RegFifo(UInt(blockWidth.W), queueDepth)) // NOTE: This requires a lot of registers
  val tagFifo = Module(new RegFifo(UInt(tagWidth.W), queueDepth))
  val indexFifo = Module(new RegFifo(UInt(indexWidth.W), queueDepth))
  val isCritFifo = Module(new RegFifoWithStatus(Bool(), queueDepth))

  val queueFull = !wbDataFifo.io.enq.ready || !tagFifo.io.enq.ready || !indexFifo.io.enq.ready || !isCritFifo.io.enq.ready
  val queueEmpty = !wbDataFifo.io.deq.valid || !tagFifo.io.deq.valid || !indexFifo.io.deq.valid || !isCritFifo.io.deq.valid

  wbDataFifo.io.enq.valid := io.push.push
  tagFifo.io.enq.valid := io.push.push
  indexFifo.io.enq.valid := io.push.push
  isCritFifo.io.enq.valid := io.push.push
  tagFifo.io.enq.bits := io.push.pushEntry.tag
  indexFifo.io.enq.bits := io.push.pushEntry.index
  wbDataFifo.io.enq.bits := io.push.pushEntry.wbData
  isCritFifo.io.enq.bits := io.push.pushEntry.isCrit

  wbDataFifo.io.deq.ready := io.pop.pop
  tagFifo.io.deq.ready := io.pop.pop
  indexFifo.io.deq.ready := io.pop.pop
  isCritFifo.io.deq.ready := io.pop.pop
  io.pop.popEntry.tag := tagFifo.io.deq.bits
  io.pop.popEntry.index := indexFifo.io.deq.bits
  io.pop.popEntry.wbData := wbDataFifo.io.deq.bits
  io.pop.popEntry.isCrit := isCritFifo.io.deq.bits

  io.isFirstInQCrit := isCritFifo.io.deq.bits

  io.push.full := queueFull
  io.pop.empty := queueEmpty

  io.wbInfo.wbQueueCritReqs := isCritFifo.io.regOut
  io.wbInfo.wbQueueValidReqs := isCritFifo.io.validRegs
}

class WriteBackFifo(queueDepth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int, enCritWb: Boolean = false) extends Module {
  val io = IO(new WbFifoIO(tagWidth, indexWidth, blockWidth, queueDepth))

  if (enCritWb) {
    val nonCritQueue = Module(new WbQueue(queueDepth, tagWidth, indexWidth, blockWidth))
    val critQueue = Module(new WbQueue(queueDepth, tagWidth, indexWidth, blockWidth))

    // Demultiplexer for pushing entries
    nonCritQueue.io.push <> 0.U.asTypeOf(nonCritQueue.io.push)
    critQueue.io.push <> 0.U.asTypeOf(critQueue.io.push)

    when(io.pushCrit === 0.U) {
      nonCritQueue.io.push <> io.push
    }.otherwise {
      critQueue.io.push <> io.push
    }

    // Multiplexer for popping entries
    nonCritQueue.io.pop <> 0.U.asTypeOf(nonCritQueue.io.pop)
    critQueue.io.pop <> 0.U.asTypeOf(critQueue.io.pop)

    when(io.popQSel === 0.U) {
      io.pop <> nonCritQueue.io.pop
    }.otherwise {
      io.pop <> critQueue.io.pop
    }

    io.wbInfo <> nonCritQueue.io.wbInfo
    io.critEmpty := critQueue.io.pop.empty
    io.nonCritEmpty := nonCritQueue.io.pop.empty
  } else {
    val nonCritQueue = Module(new WbQueue(queueDepth, tagWidth, indexWidth, blockWidth))

    nonCritQueue.io.push <> io.push
    io.pop <> nonCritQueue.io.pop
    io.wbInfo <> nonCritQueue.io.wbInfo
    io.critEmpty := true.B
    io.nonCritEmpty := nonCritQueue.io.pop.empty
  }
}