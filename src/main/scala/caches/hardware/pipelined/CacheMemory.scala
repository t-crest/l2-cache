package caches.hardware.pipelined

import caches.hardware.util._
import chisel3._
import chisel3.util._

/**
 * Cache memory module with byte-level write enable.
 *
 * @param sizeInBytes      Size of the cache memory in bytes
 * @param nWays            Number of ways (associativity) in the cache
 * @param bytesPerBlock    Number of bytes per cache block
 * @param bytesPerSubBlock Number of bytes per sub-block
 */
class CacheMemory(sizeInBytes: Int, nWays: Int, bytesPerBlock: Int, bytesPerSubBlock: Int) extends Module {
  private val nSets = sizeInBytes / (nWays * bytesPerBlock)
  private val nSubBlocks = bytesPerBlock / bytesPerSubBlock
  private val indexWidth = log2Up(nSets)

  val io = IO(new Bundle {
    val stall = Input(Bool())
    val rIndex = Input(UInt(indexWidth.W))
    val rWayIdx = Input(UInt(log2Up(nWays).W))
    val wrIndex = Input(UInt(indexWidth.W))
    val wrWayIdx = Input(UInt(log2Up(nWays).W))
    val wrEn = Input(Bool())
    val wrData = Input(Vec(nSubBlocks, UInt((bytesPerSubBlock * 8).W)))
    val byteMask = Input(UInt(bytesPerBlock.W))
    val rData = Output(Vec(nSubBlocks, UInt((bytesPerSubBlock * 8).W)))
  })

  val subBlockSplitMem = Array.fill(nSubBlocks)(
    Array.fill(bytesPerSubBlock)(
      Module(new MemBlock(nSets * nWays, 8, forward = false))
    )
  )

  val rData = VecInit(
    Seq.fill(nSubBlocks)(VecInit(
      Seq.fill(bytesPerSubBlock)(0.U(8.W))
    ))
  )

  val byteEnMask = io.byteMask.asBools
  val rAddr = Cat(io.rWayIdx, io.rIndex)
  val wrAddr = Cat(io.wrWayIdx, io.wrIndex)

  for (subBlockIdx <- 0 until nSubBlocks) {
    for (byteIdx <- 0 until bytesPerSubBlock) {
      val subBlockBytesWrData = io.wrData(subBlockIdx)(7 + byteIdx * 8, byteIdx * 8)
      val subBlockByteWrEn = io.wrEn && byteEnMask(byteIdx + (subBlockIdx * bytesPerSubBlock))

      subBlockSplitMem(subBlockIdx)(byteIdx).io.readAddr := rAddr // Needs to be index + way offset
      subBlockSplitMem(subBlockIdx)(byteIdx).io.writeData := subBlockBytesWrData
      subBlockSplitMem(subBlockIdx)(byteIdx).io.writeAddr := wrAddr // Needs to be index + way offset
      subBlockSplitMem(subBlockIdx)(byteIdx).io.wrEn := subBlockByteWrEn
      subBlockSplitMem(subBlockIdx)(byteIdx).io.stall := io.stall

      rData(subBlockIdx)(byteIdx) := subBlockSplitMem(subBlockIdx)(byteIdx).io.readData
    }
  }

  for (subBlockIdx <- 0 until nSubBlocks) {
    io.rData(subBlockIdx) := rData(subBlockIdx).asUInt
  }
}