package caches.hardware.pipelined

import caches.hardware.util._
import chisel3._
import chisel3.util._

class CacheCmdIO(nCores: Int, reqIdWidth: Int, blockOffsetWidth: Int) extends Bundle {
  val coreIdWidth = log2Up(nCores)

  val reqId = Input(UInt(reqIdWidth.W))
  val coreId = Input(UInt(coreIdWidth.W))
  val blockOffset = Input(UInt(blockOffsetWidth.W))
}

class LineRequestIO(nCores: Int, nWays: Int, tagWidth: Int, indexWidth: Int, dataWidth: Int) extends Bundle {
  val tag = Input(UInt(tagWidth.W))
  val index = Input(UInt(indexWidth.W))
  val byteEn = Input(UInt((dataWidth / 8).W))
  val incidentCoreId = Input(UInt(log2Up(nCores).W)) // The core that caused this miss
  val isCrit = Input(Bool())
  val replaceWay = Input(UInt(log2Up(nWays).W))
}

class MshrInfoIO(nCores: Int, nMshrs: Int, nWays: Int, indexWidth: Int, tagWidth: Int) extends Bundle {
  val currentIndexes = Output(Vec(nMshrs, UInt(indexWidth.W)))
  val currentTags = Output(Vec(nMshrs, UInt(tagWidth.W)))
  val replacementWays = Output(Vec(nMshrs, UInt(log2Up(nWays).W)))
  val incidentCoreIds = Output(Vec(nMshrs, UInt(log2Up(nCores).W)))
  val critMshrs = Output(Vec(nMshrs, Bool()))
  val validMshrs = Output(Vec(nMshrs, Bool()))
  val fullCmds = Output(Vec(nMshrs, Bool()))
  val wrPtr = Output(UInt(log2Ceil(nMshrs).W))
  val elementCnt = Output(UInt(log2Up(nMshrs + 1).W))
}

class MshrPushIO(nCores: Int, nMshrs: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, subBlockWidth: Int) extends Bundle() {
  // For inserting a new MSHR entry
  val pushReq = Input(Bool())
  val pushReqEntry = new LineRequestIO(nCores, nWays, tagWidth, indexWidth, subBlockWidth)
  // For pushing new command into MSHR entry
  val pushCmd = Input(Bool())
  val mshrIdx = Input(UInt(log2Up(nMshrs).W))
  val pushCmdEntry = new CacheCmdIO(nCores, reqIdWidth, blockOffsetWidth)
  // For updating the byte enable mask of the MSHR entry
  val updateByteEn = Input(Bool())
  val updateByteEnRow = Input(UInt(log2Up(nMshrs).W))
  val updateByteEnCol = Input(UInt(blockOffsetWidth.W))
  val updateByteEnVal = Input(UInt((subBlockWidth / 8).W))
  // Info about the current state of the MSHR array
  val full = Output(Bool())
}

class MshrPopIO(nCores: Int, nCmds: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, blockWidth: Int) extends Bundle() {
  val pop = Input(Bool())
  val reading = Input(Bool()) // Indicate that the memory interface is reading the pop entry and that no new data should be added to it
  val popEntry = Flipped(new LineRequestIO(nCores, nWays, tagWidth, indexWidth, blockWidth))
  val cmds = Output(Vec(nCmds, new CacheCmdIO(nCores, reqIdWidth, blockOffsetWidth)))
  val cmdCnt = Output(UInt((log2Up(nCmds) + 1).W))
  val empty = Output(Bool())
}

class MshrIO(nCores: Int, nMSHRs: Int, nCmds: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Bundle {
  val info = new MshrInfoIO(nCores, nMSHRs, nWays, indexWidth, tagWidth)
  val push = new MshrPushIO(nCores, nMSHRs, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, subBlockWidth)
  val pop = new MshrPopIO(nCores, nCmds, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, blockWidth)
}

class MissFifoIO(nCores: Int, nMSHRs: Int, nCmds: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Bundle {
  val nonCritInfo = new MshrInfoIO(nCores, nMSHRs, nWays, indexWidth, tagWidth)
  val critInfo = new MshrInfoIO(nCores, nMSHRs, nWays, indexWidth, tagWidth)
  val pushCrit = Input(Bool()) // Indicate if the current request should be pushed to the critical queue
  val popQSel = Input(Bool()) // Signal for selecting which queue to pop an entry from
  val push = new MshrPushIO(nCores, nMSHRs, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, subBlockWidth)
  val pop = new MshrPopIO(nCores, nCmds, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, blockWidth)
  val critEmpty = Output(Bool())
  val nonCritEmpty = Output(Bool())
  val full = Output(Bool())
}

class MshrPushDemux(nCores: Int, nCmds: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, blockWidth: Int) extends Module {
  val io = IO(new Bundle {
    val sel = Input(UInt(1.W))
    val in = new MshrPushIO(nCores, nCmds, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, blockWidth)
    val out1 = Flipped(new MshrPushIO(nCores, nCmds, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, blockWidth))
    val out2 = Flipped(new MshrPushIO(nCores, nCmds, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, blockWidth))
  })

  io.out2 <> 0.U.asTypeOf(io.out2)
  io.out1 <> 0.U.asTypeOf(io.out1)

  when (io.sel === 0.U) {
    io.out1 <> io.in
  } .otherwise {
    io.out2 <> io.in
  }
}

class MshrPopMux(nCores: Int, nCmds: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, blockWidth: Int) extends Module {
  val io = IO(new Bundle {
    val sel = Input(UInt(1.W))
    val in1 = Flipped(new MshrPopIO(nCores, nCmds, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, blockWidth))
    val in2 = Flipped(new MshrPopIO(nCores, nCmds, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, blockWidth))
    val out = new MshrPopIO(nCores, nCmds, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, blockWidth)
  })

  when (io.sel === 0.U) {
    io.in1.pop := io.out.pop
    io.in1.reading := io.out.reading
    io.out.popEntry := io.in1.popEntry
    io.out.cmds := io.in1.cmds
    io.out.cmdCnt := io.in1.cmdCnt
    io.out.empty := io.in1.empty

    io.in2.pop := false.B
    io.in2.reading := false.B
  } .otherwise {
    io.in2.pop := io.out.pop
    io.in2.reading := io.out.reading
    io.out.popEntry := io.in2.popEntry
    io.out.cmds := io.in2.cmds
    io.out.cmdCnt := io.in2.cmdCnt
    io.out.empty := io.in2.empty

    io.in1.pop := false.B
    io.in1.reading := false.B
  }
}

class RequestMshrQueue(nCores: Int, nMshrs: Int, nWays: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Module {
  val io = IO(new Bundle {
    val push = Input(Bool())
    val pushEntry = new LineRequestIO(nCores, nWays, tagWidth, indexWidth, blockWidth)
    val blockOffset = Input(UInt(log2Up(blockWidth / subBlockWidth).W))
    val updateByteEn = Input(Bool())
    val updateByteEnRow = Input(UInt(log2Up(nMshrs).W))
    val updateByteEnCol = Input(UInt(log2Up(blockWidth / subBlockWidth).W))
    val updateByteEnVal = Input(UInt((subBlockWidth / 8).W))
    val pop = Input(Bool())
    val popEntry = Flipped(new LineRequestIO(nCores, nWays, tagWidth, indexWidth, blockWidth))
    val empty = Output(Bool())
    val full = Output(Bool())
    val wrPtr = Output(UInt(log2Up(nMshrs).W))
    val rdPtr = Output(UInt(log2Up(nMshrs).W))
    val currentIndexes = Output(Vec(nMshrs, UInt(indexWidth.W)))
    val currentTags = Output(Vec(nMshrs, UInt(tagWidth.W)))
    val replacementWays = Output(Vec(nMshrs, UInt(log2Up(nWays).W)))
    val incidentCoreIds = Output(Vec(nMshrs, UInt(log2Up(nCores).W)))
    val isCritCores = Output(Vec(nMshrs, Bool()))
    val validMSHRs = Output(Vec(nMshrs, Bool()))
  })

  val nSubBlocks = blockWidth / subBlockWidth

  val tagQueue = Module(new RegFifoWithStatus(UInt(tagWidth.W), nMshrs))
  val idxQueue = Module(new RegFifoWithStatus(UInt(indexWidth.W), nMshrs))
  val wayQueue = Module(new RegFifoWithStatus(UInt(log2Up(nWays).W), nMshrs))
  val incidentCoreQueue = Module(new RegFifoWithStatus(UInt(log2Up(nCores).W), nMshrs))
  val isCritQueue = Module(new RegFifoWithStatus(UInt(log2Up(nCores).W), nMshrs)) // Whether the incident core is critical or not
  val currWrPtr = tagQueue.io.wrPtr
  val currRdPtr = tagQueue.io.rdPtr

  val byteEnQueue = Module(new BlockRegFifoWithUpdate(blockWidth / 8, nSubBlocks, nMshrs,
    updateFun = (newData, oldData) => {
      // Update the byte enable mask for the sub-blocks
      val newByteEn = WireDefault(oldData)
      newByteEn := oldData | newData
      newByteEn
    }
  ))

  val full = !tagQueue.io.enq.ready || !idxQueue.io.enq.ready
  val empty = !tagQueue.io.deq.valid || !idxQueue.io.deq.valid

  // Connections for pushing data
  tagQueue.io.enq.valid := io.push
  tagQueue.io.enq.bits := io.pushEntry.tag

  idxQueue.io.enq.valid := io.push
  idxQueue.io.enq.bits := io.pushEntry.index

  wayQueue.io.enq.valid := io.push
  wayQueue.io.enq.bits := io.pushEntry.replaceWay

  incidentCoreQueue.io.enq.valid := io.push
  incidentCoreQueue.io.enq.bits := io.pushEntry.incidentCoreId

  isCritQueue.io.enq.valid := io.push
  isCritQueue.io.enq.bits := io.pushEntry.isCrit

  val byteShift = Cat(io.blockOffset, 0.U(log2Up(subBlockWidth / 8).W))
  val byteMask = (io.pushEntry.byteEn << byteShift).asUInt
  val blockAlignedByteEn = byteMask((blockWidth / 8) - 1, 0)

  byteEnQueue.io.push := io.push
  byteEnQueue.io.update := io.updateByteEn
  byteEnQueue.io.rdPtr := currRdPtr
  byteEnQueue.io.wrPtr := currWrPtr
  byteEnQueue.io.wrData := blockAlignedByteEn
  byteEnQueue.io.updtPtr := io.updateByteEnRow
  byteEnQueue.io.updtBlockIdx := io.updateByteEnCol
  byteEnQueue.io.updtData := io.updateByteEnVal

  // Connections for popping data
  wayQueue.io.deq.ready := io.pop
  tagQueue.io.deq.ready := io.pop
  idxQueue.io.deq.ready := io.pop
  incidentCoreQueue.io.deq.ready := io.pop
  isCritQueue.io.deq.ready := io.pop

  io.popEntry.tag := tagQueue.io.deq.bits
  io.popEntry.index := idxQueue.io.deq.bits
  io.popEntry.replaceWay := wayQueue.io.deq.bits
  io.popEntry.byteEn := byteEnQueue.io.rdData
  io.popEntry.incidentCoreId := incidentCoreQueue.io.deq.bits
  io.popEntry.isCrit := isCritQueue.io.deq.bits

  io.empty := empty
  io.full := full

  io.rdPtr := currRdPtr
  io.wrPtr := currWrPtr

  io.validMSHRs := tagQueue.io.validRegs
  io.currentTags := tagQueue.io.regOut
  io.currentIndexes := idxQueue.io.regOut
  io.replacementWays := wayQueue.io.regOut
  io.incidentCoreIds := incidentCoreQueue.io.regOut
  io.isCritCores := isCritQueue.io.regOut
}

class CmdMshrQueue(nCmds: Int, nCores: Int, nMshrs: Int, reqIdWidth: Int, blockOffsetWidth: Int) extends Module {
  val cmdWidth = log2Up(nCores) + reqIdWidth + blockOffsetWidth

  val io = IO(new Bundle {
    val push = Input(Bool())
    val update = Input(Bool())
    val rdPtr = Input(UInt(log2Up(nMshrs).W))
    val wrPtr = Input(UInt(log2Up(nMshrs).W))
    val wrData = Input(UInt((cmdWidth * nCmds).W))
    val updtPtr = Input(UInt(log2Up(nMshrs).W))
    val updtData = Input(UInt(cmdWidth.W))
    val rdCmds = Output(Vec(nCmds, new CacheCmdIO(nCores, reqIdWidth, blockOffsetWidth)))
    val cmdCnt = Output(UInt((log2Up(nCmds) + 1).W))
    val full = Output(Vec(nMshrs, Bool()))
  })

  val cntRegs = RegInit(VecInit(Seq.fill(nMshrs)(0.U((log2Up(nCmds) + 1).W))))

  when(io.push) {
    cntRegs(io.wrPtr) := 1.U
  }.elsewhen(io.update) {
    cntRegs(io.updtPtr) := cntRegs(io.updtPtr) + 1.U
  }

  val cmdBlockQueue = Module(new BlockRegFifoWithUpdate(cmdWidth * nCmds, nCmds, nMshrs))

  cmdBlockQueue.io.push := io.push
  cmdBlockQueue.io.update := io.update
  cmdBlockQueue.io.rdPtr := io.rdPtr
  cmdBlockQueue.io.wrPtr := io.wrPtr
  cmdBlockQueue.io.wrData := io.wrData
  cmdBlockQueue.io.updtPtr := io.updtPtr
  cmdBlockQueue.io.updtBlockIdx := cntRegs(io.updtPtr)
  cmdBlockQueue.io.updtData := io.updtData

  for (mshrIdx <- 0 until nMshrs) {
    io.full(mshrIdx) := cntRegs(mshrIdx) === nCmds.U
  }

  for (cmdIdx <- 0 until nCmds) {
    val cmdOffset = cmdIdx * cmdWidth
    io.rdCmds(cmdIdx).blockOffset := cmdBlockQueue.io.rdData(blockOffsetWidth - 1 + cmdOffset, cmdOffset)
    io.rdCmds(cmdIdx).coreId := cmdBlockQueue.io.rdData(log2Up(nCores) - 1 + blockOffsetWidth + cmdOffset, blockOffsetWidth + cmdOffset)
    io.rdCmds(cmdIdx).reqId := cmdBlockQueue.io.rdData(reqIdWidth - 1 + log2Up(nCores) + blockOffsetWidth + cmdOffset, log2Up(nCores) + blockOffsetWidth + cmdOffset)
  }

  io.cmdCnt := cntRegs(io.rdPtr)
}

class MshrQueue(nCores: Int, nCmds: Int, nMshrs: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, subBlockWidth: Int, blockWidth: Int) extends Module {
  val io = IO(new MshrIO(nCores = nCores, nMSHRs = nMshrs, nCmds = nCmds, nWays = nWays, reqIdWidth = reqIdWidth, tagWidth = tagWidth, indexWidth = indexWidth, blockOffsetWidth = blockOffsetWidth, blockWidth = blockWidth, subBlockWidth = subBlockWidth))

  val reqQueue = Module(new RequestMshrQueue(nCores, nMshrs, nWays, tagWidth, indexWidth, blockWidth, subBlockWidth))
  val cmdQueue = Module(new CmdMshrQueue(nCmds, nCores, nMshrs, reqIdWidth, blockOffsetWidth))

  reqQueue.io.push := io.push.pushReq
  reqQueue.io.pushEntry := io.push.pushReqEntry
  reqQueue.io.blockOffset := io.push.pushCmdEntry.blockOffset
  reqQueue.io.updateByteEn := io.push.updateByteEn
  reqQueue.io.updateByteEnRow := io.push.updateByteEnRow
  reqQueue.io.updateByteEnCol := io.push.updateByteEnCol
  reqQueue.io.updateByteEnVal := io.push.updateByteEnVal
  reqQueue.io.pop := io.pop.pop

  cmdQueue.io.push := io.push.pushReq
  cmdQueue.io.update := io.push.pushCmd
  cmdQueue.io.rdPtr := reqQueue.io.rdPtr
  cmdQueue.io.wrPtr := reqQueue.io.wrPtr
  cmdQueue.io.wrData := Cat(io.push.pushCmdEntry.reqId, io.push.pushCmdEntry.coreId, io.push.pushCmdEntry.blockOffset)
  cmdQueue.io.updtPtr := io.push.mshrIdx
  cmdQueue.io.updtData := Cat(io.push.pushCmdEntry.reqId, io.push.pushCmdEntry.coreId, io.push.pushCmdEntry.blockOffset)

  val queueElementCntReg = RegInit(0.U(log2Up(nMshrs + 1).W))
  when(io.push.pushReq && !io.pop.pop) {
    queueElementCntReg := queueElementCntReg + 1.U
  }.elsewhen(!io.push.pushReq && io.pop.pop) {
    queueElementCntReg := queueElementCntReg - 1.U
  }

  val validMshrs = VecInit(Seq.fill(nMshrs)(false.B))
  validMshrs := reqQueue.io.validMSHRs

  when(io.pop.reading) {
    validMshrs(reqQueue.io.rdPtr) := false.B
  }

  io.push.full := reqQueue.io.full

  io.info.wrPtr := reqQueue.io.wrPtr
  io.info.validMshrs := validMshrs
  io.info.currentTags := reqQueue.io.currentTags
  io.info.currentIndexes := reqQueue.io.currentIndexes
  io.info.replacementWays := reqQueue.io.replacementWays
  io.info.incidentCoreIds := reqQueue.io.incidentCoreIds
  io.info.critMshrs := reqQueue.io.isCritCores
  io.info.fullCmds := cmdQueue.io.full
  io.info.elementCnt := queueElementCntReg

  io.pop.empty := reqQueue.io.empty
  io.pop.cmdCnt := cmdQueue.io.cmdCnt
  io.pop.popEntry := reqQueue.io.popEntry
  io.pop.cmds := cmdQueue.io.rdCmds
}

class MissFifo(nCores: Int, nCmds: Int, nMshrs: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, subBlockWidth: Int, blockWidth: Int, enCritMisses: Boolean = false) extends Module {
  val io = IO(new MissFifoIO(nCores, nMshrs, nCmds, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, blockWidth, subBlockWidth))

  if (enCritMisses) {
    val critQueue = Module(new MshrQueue(nCores, nCmds, nMshrs, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, subBlockWidth, blockWidth))
    val nonCritQueue = Module(new MshrQueue(nCores, nCmds, nMshrs, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, subBlockWidth, blockWidth))

    // De-multiplex the push interface to the two queues
    val mshrPushDemux = Module(new MshrPushDemux(nCores, nCmds, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, blockWidth))

    mshrPushDemux.io.sel := io.pushCrit
    mshrPushDemux.io.in <> io.push
    nonCritQueue.io.push <> mshrPushDemux.io.out1
    critQueue.io.push <> mshrPushDemux.io.out2

    // Multiplex between the two queues for popping
    val mshrPopDemux = Module(new MshrPopMux(nCores, nCmds, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, blockWidth))

    // Choose to always pop the critical queue as long as it is not empty
    mshrPopDemux.io.sel := io.popQSel
    mshrPopDemux.io.in1 <> nonCritQueue.io.pop
    mshrPopDemux.io.in2 <> critQueue.io.pop
    io.pop <> mshrPopDemux.io.out

    io.critInfo <> critQueue.io.info
    io.nonCritInfo <> nonCritQueue.io.info

    // Since the critical queue is always given priority over non-critical queue, a hazards occurs when a replacement policy
    // instructs a non-critical request to evict a way that then a critical request that has reached contention limit is
    // told to evict too. For instance, a non-critical request can evict way 2, so it is pushed to non-critical fifo; then
    // a critical request whose core has reached contention limit is told to evict the same way: 2 (most likely since any
    // other ways are already owned by critical cores); then the critical way evicts this line first followed by a
    // non-critical way evicting this line later on. This creates a additional contention, since if this line is later on
    // needed by a critical core, it will have to refetch again, thus resulting in two line accesses from the main memory
    // for a critical core.
    // NOTE: This is a rather rare case.

    val anyMatchingReqsInNonCrit = VecInit(Seq.fill(nMshrs)(false.B))
    val critPopValid = !critQueue.io.pop.empty
    for (mshrIdx <- 0 until nMshrs) {
      val nonCritValid = nonCritQueue.io.info.validMshrs(mshrIdx)
      val conflict = critPopValid && nonCritValid && nonCritQueue.io.info.currentIndexes(mshrIdx) === critQueue.io.pop.popEntry.index && nonCritQueue.io.info.replacementWays(mshrIdx) === critQueue.io.pop.popEntry.replaceWay
      anyMatchingReqsInNonCrit(mshrIdx) := conflict
    }

    val queueConflict = anyMatchingReqsInNonCrit.reduce((x, y) => x || y)

    io.full := critQueue.io.push.full || nonCritQueue.io.push.full
    io.critEmpty := critQueue.io.pop.empty || queueConflict
    io.nonCritEmpty := nonCritQueue.io.pop.empty
  } else {
    val nonCritQueue = Module(new MshrQueue(nCores, nCmds, nMshrs, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, subBlockWidth, blockWidth))

    nonCritQueue.io.push <> io.push
    io.pop <> nonCritQueue.io.pop

    io.critInfo <> 0.U.asTypeOf(io.critInfo)
    io.nonCritInfo <> nonCritQueue.io.info

    io.full := nonCritQueue.io.push.full
    io.critEmpty := true.B
    io.nonCritEmpty := nonCritQueue.io.pop.empty
  }
}