package caches.hardware.pipelined.stages

import caches.hardware.pipelined.CacheResponseIO
import chisel3._
import chisel3.util._

/**
 * IO interface from the read stage to the update stage
 */
class UpdateIO(nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Bundle {
  val valid = Input(Bool())
  val isHit = Input(Bool())
  val reqId = Input(UInt(reqIdWidth.W))
  val coreId = Input(UInt(log2Up(nCores).W))
  val rw = Input(Bool())
  val wData = Input(UInt(subBlockWidth.W))
  val byteEn = Input(UInt((blockWidth / 8).W))
  val wWay = Input(UInt(log2Up(nWays).W))
  val repWay = Input(UInt(log2Up(nWays).W))
  val responseStatus = Input(UInt(1.W))
  val tag = Input(UInt(tagWidth.W))
  val index = Input(UInt(indexWidth.W))
  val blockOffset = Input(UInt(log2Up(blockWidth / subBlockWidth).W))
  val memReadData = Input(Vec(blockWidth / subBlockWidth, UInt(subBlockWidth.W)))
}

/**
 * IO interface from the memory interface to the update stage
 */
class MemInterfaceToUpdateIO(nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Bundle() {
  val valid = Input(Bool())
  val validCmd = Input(Bool())
  val byteEn = Input(UInt((blockWidth / 8).W))
  val reqId = Input(UInt(reqIdWidth.W))
  val coreId = Input(UInt(log2Up(nCores).W))
  val wWay = Input(UInt(log2Up(nWays).W))
  val tag = Input(UInt(tagWidth.W))
  val index = Input(UInt(indexWidth.W))
  val blockOffset = Input(UInt(log2Up(blockWidth / subBlockWidth).W))
  val memReadData = Input(Vec(blockWidth / subBlockWidth, UInt(subBlockWidth.W)))
}

class SetLineValidIO(nWays: Int, indexWidth: Int, tagWidth: Int) extends Bundle {
  val refill = Output(Bool()) // For setting the line as valid
  val tag = Output(UInt(tagWidth.W))
  val index = Output(UInt(indexWidth.W))
  val way = Output(UInt(log2Up(nWays).W))
}

/**
 * IO interface from the update stage to the tag stage
 */
class TagUpdateIO(nWays: Int, indexWidth: Int, tagWidth: Int) extends SetLineValidIO(nWays, indexWidth, tagWidth) {}

/**
 * IO interface for control signals from the update stage to the cache memory
 */
class CacheMemUpdateIO(nWays: Int, indexWidth: Int, nSubBlocks: Int, subBlockWidth: Int) extends Bundle() {
  val wrEn = Output(Bool())
  val way = Output(UInt(log2Up(nWays).W))
  val index = Output(UInt(indexWidth.W))
  val memWriteData = Output(Vec(nSubBlocks, UInt(subBlockWidth.W)))
  val byteMask = Output(UInt(((nSubBlocks * subBlockWidth) / 8).W))
}

class UpdateUnit(nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Module {
  private val nSubBlocks = blockWidth / subBlockWidth

  val io = IO(new Bundle {
    val readStage = new UpdateIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockWidth, subBlockWidth)
    val memoryInterface = new MemInterfaceToUpdateIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockWidth, subBlockWidth)
    val coreResp = new CacheResponseIO(subBlockWidth, reqIdWidth)
    val memUpdate = new CacheMemUpdateIO(nWays, indexWidth, nSubBlocks, subBlockWidth)
    val tagUpdate = new TagUpdateIO(nWays, indexWidth, tagWidth)
    val pipeStall = Input(Bool())
    val outCoreId = Output(UInt(log2Up(nCores).W))
    val stall = Output(Bool())
    val setValidLine = new SetLineValidIO(nWays, indexWidth, tagWidth)
  })

  val refill = WireDefault(false.B)
  val stall = WireDefault(false.B)
  val wrEn = WireDefault(false.B)

  val updateTag = WireDefault(0.U(tagWidth.W))
  val updateIndex = WireDefault(0.U(indexWidth.W))
  val updateWay = WireDefault(0.U(log2Up(nWays).W))
  val coreRespCoreId = WireDefault(0.U(log2Up(nCores).W))

  val coreRespValid = WireDefault(false.B)
  val coreRespId = WireDefault(0.U(reqIdWidth.W))
  val coreRespData = WireDefault(0.U(subBlockWidth.W))
  val coreRespStatus = WireDefault(0.U(1.W))
  val updateWriteData = VecInit(Seq.fill(nSubBlocks)(0.U(subBlockWidth.W)))
  val updateWriteByteMask = WireDefault(0.U((blockWidth / 8).W))

  when(io.memoryInterface.valid) {
    stall := true.B

    updateTag := io.memoryInterface.tag
    updateIndex := io.memoryInterface.index
    updateWay := io.memoryInterface.wWay

    coreRespId := io.memoryInterface.reqId
    coreRespCoreId := io.memoryInterface.coreId
    coreRespData := io.memoryInterface.memReadData(io.memoryInterface.blockOffset)
    coreRespStatus := 1.U

    updateWriteData := io.memoryInterface.memReadData
    updateWriteByteMask := ~io.memoryInterface.byteEn

    when(io.memoryInterface.validCmd) {
      coreRespValid := true.B // Respond to the request if it is a valid command
    }.otherwise {
      refill := true.B
      wrEn := true.B
    }
  }.elsewhen(io.readStage.valid) {
    when(io.readStage.isHit) {
      updateTag := io.readStage.tag
      updateIndex := io.readStage.index
      updateWay := io.readStage.wWay

      coreRespId := io.readStage.reqId
      coreRespCoreId := io.readStage.coreId
      coreRespData := io.readStage.memReadData(io.readStage.blockOffset)
      coreRespStatus := io.readStage.responseStatus
      coreRespValid := true.B && !io.pipeStall

      when(io.readStage.rw) {
        wrEn := true.B

        updateWriteByteMask := io.readStage.byteEn
        updateWriteData(io.readStage.blockOffset) := io.readStage.wData
      }
    }.otherwise {
      updateTag := io.readStage.tag
      updateIndex := io.readStage.index
      updateWay := io.readStage.repWay

      // If it is a wr request and a miss we invalidate the cache line, store the wData, and
      // wait for the remainder of the line to be brought in
      when(io.readStage.rw && (io.readStage.responseStatus === 1.U)) { // Disallow writing when rejected
        wrEn := true.B

        updateWriteByteMask := io.readStage.byteEn
        updateWriteData(io.readStage.blockOffset) := io.readStage.wData
      }
    }
  }

  when(io.memoryInterface.valid && io.readStage.valid) {
    stall := true.B
  }

  io.stall := stall

  io.tagUpdate.tag := updateTag
  io.tagUpdate.index := updateIndex
  io.tagUpdate.way := updateWay
  io.tagUpdate.refill := refill

  io.setValidLine.tag := RegNext(updateTag)
  io.setValidLine.index := RegNext(updateIndex)
  io.setValidLine.way := RegNext(updateWay)
  io.setValidLine.refill := RegNext(refill)

  io.memUpdate.wrEn := wrEn
  io.memUpdate.way := updateWay
  io.memUpdate.index := updateIndex
  io.memUpdate.memWriteData := updateWriteData
  io.memUpdate.byteMask := updateWriteByteMask

  io.coreResp.reqId.valid := coreRespValid
  io.coreResp.reqId.bits := coreRespId
  io.coreResp.rData := coreRespData
  io.outCoreId := coreRespCoreId
}
