package caches.hardware.pipelined.stages

import caches.hardware.util._
import chisel3._
import chisel3.util._

class DecIO(nCores: Int, reqIdWidth: Int, addrWidth: Int, subBlockWidth: Int) extends Bundle() {
  val coreId = Input(UInt(log2Up(nCores).W))
  val reqValid = Input(Bool())
  val reqId = Input(UInt(reqIdWidth.W))
  val reqRw = Input(Bool())
  val wData = Input(UInt(subBlockWidth.W))
  val byteEn = Input(UInt((subBlockWidth / 8).W))
  val addr = Input(UInt(addrWidth.W))
}

class Dec(nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffWidth: Int, byteOffWidth: Int, subBlockWidth: Int) extends Module() {
  private val addrWidth = tagWidth + indexWidth + blockOffWidth + byteOffWidth

  val io = IO(new Bundle {
    val dec = new DecIO(nCores, reqIdWidth, addrWidth, subBlockWidth)
    val tag = Flipped(new TagIO(nCores, reqIdWidth, tagWidth, indexWidth, blockOffWidth, subBlockWidth))
    val stall = Input(Bool())
  })

  // val byteOffset = io.addr(byteOffsetWidth - 1, 0)
  val blockOffset = io.dec.addr((blockOffWidth - 1) + byteOffWidth, byteOffWidth)
  val index = io.dec.addr((indexWidth - 1) + blockOffWidth + byteOffWidth, blockOffWidth + byteOffWidth)
  val tag = io.dec.addr(addrWidth - 1, indexWidth + blockOffWidth + byteOffWidth)

  io.tag.coreId := PipelineReg(io.dec.coreId, 0.U, !io.stall)
  io.tag.reqValid := PipelineReg(io.dec.reqValid, false.B, !io.stall)
  io.tag.reqId := PipelineReg(io.dec.reqId, 0.U, !io.stall)
  io.tag.reqRw := PipelineReg(io.dec.reqRw, false.B, !io.stall)
  io.tag.wData := PipelineReg(io.dec.wData, 0.U, !io.stall)
  io.tag.byteEn := PipelineReg(io.dec.byteEn, 0.U, !io.stall)
  io.tag.blockOffset := PipelineReg(blockOffset, 0.U, !io.stall)
  io.tag.index := PipelineReg(index, 0.U, !io.stall)
  io.tag.tag := PipelineReg(tag, 0.U, !io.stall)
  io.tag.readIndex := index
}
