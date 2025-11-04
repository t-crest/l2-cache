package caches.hardware.pipelined.stages

import caches.hardware.pipelined.{CacheMemory, WbFifoPushIO}
import caches.hardware.util.PipelineReg
import chisel3._
import chisel3.util._

class DirtyCtrlIO(nWays: Int, indexWidth: Int) extends Bundle {
  val set = Output(Bool())
  val unset = Output(Bool())
  val wWay = Output(UInt(log2Up(nWays).W))
  val wIndex = Output(UInt(indexWidth.W))
}

class ReadIO(nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Bundle() {
  val coreId = Input(UInt(log2Up(nCores).W))
  val wb = Input(Bool())
  val repWayAtLimit = Input(Bool())
  val isRepWayCrit = Input(Bool())
  val reqValid = Input(Bool())
  val reqId = Input(UInt(reqIdWidth.W))
  val reqRw = Input(Bool())
  val wData = Input(UInt(subBlockWidth.W))
  val byteEn = Input(UInt((blockWidth / 8).W))
  val repValid = Input(Bool())
  val isHit = Input(Bool())
  val hitWay = Input(UInt(log2Up(nWays).W))
  val repWay = Input(UInt(log2Up(nWays).W))
  val isRepDirty = Input(Bool())
  val dirtyTag = Input(UInt(tagWidth.W))
  val blockOffset = Input(UInt(blockOffWidth.W))
  val index = Input(UInt(indexWidth.W))
  val tag = Input(UInt(tagWidth.W))
}

class Read(memSizeInBytes: Int, nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Module {
  val io = IO(new Bundle {
    val read = new ReadIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffWidth, blockWidth, subBlockWidth)
    val memUpdate = Flipped(new CacheMemUpdateIO(nWays = nWays, indexWidth = indexWidth, nSubBlocks = blockWidth / subBlockWidth, subBlockWidth = subBlockWidth))
    val stall = Input(Bool())
    val wbQueue = Flipped(new WbFifoPushIO(tagWidth = tagWidth, indexWidth = indexWidth, blockWidth = blockWidth))
    val wbQueuePushCrit = Output(Bool())
    val update = Flipped(new UpdateIO(nCores = nCores, nWays = nWays, reqIdWidth = reqIdWidth, tagWidth = tagWidth, indexWidth = indexWidth, blockWidth = blockWidth, subBlockWidth = subBlockWidth))
    val dirtyCtrl = new DirtyCtrlIO(nWays = nWays, indexWidth = indexWidth)
  })
  private val nSubBlocks = blockWidth / subBlockWidth

  val dataMem = Module(new CacheMemory(memSizeInBytes, nWays, blockWidth / 8, subBlockWidth / 8))

  dataMem.io.rIndex := io.read.index
  dataMem.io.rWayIdx := Mux(io.read.isHit, io.read.hitWay, io.read.repWay)
  dataMem.io.wrIndex := io.memUpdate.index
  dataMem.io.wrWayIdx := io.memUpdate.way
  dataMem.io.wrEn := io.memUpdate.wrEn
  dataMem.io.wrData := io.memUpdate.memWriteData
  dataMem.io.byteMask := io.memUpdate.byteMask
  dataMem.io.stall := io.stall

  val wbReg = PipelineReg(io.read.wb, false.B, !io.stall)
  val coreIdReg = PipelineReg(io.read.coreId, 0.U, !io.stall)
  val repWayAtLimitReg = PipelineReg(io.read.repWayAtLimit, false.B, !io.stall)
  val isRepWayCritReg = PipelineReg(io.read.isRepWayCrit, false.B, !io.stall)
  val reqValidReg = PipelineReg(io.read.reqValid, false.B, !io.stall)
  val reqIdReg = PipelineReg(io.read.reqId, 0.U, !io.stall)
  val reqRwReg = PipelineReg(io.read.reqRw, false.B, !io.stall)
  val wDataReg = PipelineReg(io.read.wData, 0.U, !io.stall)
  val byteEnReg = PipelineReg(io.read.byteEn, 0.U, !io.stall)
  val repValidReg = PipelineReg(io.read.repValid, true.B, !io.stall)
  val isHitReg = PipelineReg(io.read.isHit, false.B, !io.stall)
  val hitWayReg = PipelineReg(io.read.hitWay, 0.U, !io.stall)
  val repWayReg = PipelineReg(io.read.repWay, 0.U, !io.stall)
  val dirtyTagReg = PipelineReg(io.read.dirtyTag, 0.U, !io.stall)
  val blockOffsetReg = PipelineReg(io.read.blockOffset, 0.U, !io.stall)
  val indexReg = PipelineReg(io.read.index, 0.U, !io.stall)
  val tagReg = PipelineReg(io.read.tag, 0.U, !io.stall)

  io.wbQueuePushCrit := isRepWayCritReg && repWayAtLimitReg
  io.wbQueue.push := wbReg && !io.stall
  io.wbQueue.pushEntry.wbData := dataMem.io.rData.asUInt
  io.wbQueue.pushEntry.tag := dirtyTagReg
  io.wbQueue.pushEntry.index := indexReg
  io.wbQueue.pushEntry.isCrit := isRepWayCritReg

  io.dirtyCtrl.unset := wbReg
  io.dirtyCtrl.set := reqValidReg && reqRwReg && (isHitReg || repValidReg)
  io.dirtyCtrl.wIndex := indexReg
  io.dirtyCtrl.wWay := Mux(isHitReg, hitWayReg, repWayReg)

  io.update.valid := PipelineReg(reqValidReg, false.B, !io.stall)
  io.update.isHit := PipelineReg(isHitReg, false.B, !io.stall)
  io.update.rw := PipelineReg(reqRwReg, false.B, !io.stall)
  io.update.coreId := PipelineReg(coreIdReg, 0.U, !io.stall)
  io.update.reqId := PipelineReg(reqIdReg, 0.U, !io.stall)
  io.update.wData := PipelineReg(wDataReg, 0.U, !io.stall)
  io.update.byteEn := PipelineReg(byteEnReg, 0.U, !io.stall)
  io.update.wWay := PipelineReg(hitWayReg, 0.U, !io.stall)
  io.update.repWay := PipelineReg(repWayReg, 0.U, !io.stall)
  io.update.responseStatus := PipelineReg(repValidReg, false.B, !io.stall)
  io.update.blockOffset := PipelineReg(blockOffsetReg, 0.U, !io.stall)
  io.update.index := PipelineReg(indexReg, 0.U, !io.stall)
  io.update.tag := PipelineReg(tagReg, 0.U, !io.stall)
  io.update.memReadData := PipelineReg(dataMem.io.rData, VecInit(Seq.fill(nSubBlocks)(0.U(subBlockWidth.W))), !io.stall)
}
