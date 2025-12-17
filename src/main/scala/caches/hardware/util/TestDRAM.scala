package caches.hardware.util

import caches.hardware.pipelined.CacheMemoryControllerIO
import chisel3._
import chisel3.util._

class TestDRAM(addrWidth: Int, blockWidth: Int, beatSize: Int, burstLen: Int, dataFile: Option[String] = None) extends Module {
  require((blockWidth * 8) > beatSize, "Block width must be greater than beat size.")

  val io = IO(Flipped(new CacheMemoryControllerIO(addrWidth, beatSize)))

  private val nBursts = burstLen
  val sIdle :: sReadBurst :: sWriteBurst :: Nil = Enum(3)

  // Registers
  val stateReg = RegInit(sIdle)
  val addrReg = RegInit(0.U(addrWidth.W))
  val burstCountReg = RegInit(0.U(log2Up(nBursts).W))

  // Default signal assignments
  val rAddrReady = WireDefault(false.B)
  val rDataValid = WireDefault(false.B)
  val rDataBits = WireDefault(0.U((beatSize * 8).W))
  val rLast = WireDefault(false.B)
  val wAddrReady = WireDefault(false.B)
  val wDataReady = WireDefault(false.B)
  val memReadAddr = WireDefault(0.U(addrWidth.W))
  val memWriteAddr = WireDefault(0.U(addrWidth.W))
  val memWriteData = WireDefault(0.U((beatSize * 8).W))
  val memWrEn = WireDefault(0.U((beatSize * 8).W))

  val mem = Module(new MemBlock(math.pow(2, addrWidth).toInt, beatSize * 8, dataFile = dataFile))

  mem.io.readAddr := memReadAddr
  mem.io.writeAddr := memWriteAddr
  mem.io.writeData := memWriteData
  mem.io.wrEn := memWrEn
  mem.io.stall := false.B

  switch(stateReg) {
    is(sIdle) {
      rAddrReady := true.B
      wAddrReady := true.B

      when(io.rChannel.rAddr.valid) {
        memReadAddr := io.rChannel.rAddr.bits
        addrReg := io.rChannel.rAddr.bits
        stateReg := sReadBurst
      } .elsewhen(io.wChannel.wAddr.valid) {
        addrReg := io.wChannel.wAddr.bits
        stateReg := sWriteBurst
      }
    }

    is(sReadBurst) {
      rDataBits := mem.io.readData
      rDataValid := true.B
      memReadAddr := addrReg

      when(io.rChannel.rData.ready) {
        val nextReadAddr = WireDefault(0.U(addrWidth.W))
        val nextBurstCount = WireDefault(0.U(nBursts.W))
        nextBurstCount := burstCountReg + 1.U
        nextReadAddr := addrReg + 1.U

        when(burstCountReg === (nBursts - 1).U) {
          rLast := true.B
          stateReg := sIdle
          nextBurstCount := 0.U
          nextReadAddr := 0.U
        }

        memReadAddr := nextReadAddr
        addrReg := nextReadAddr
        burstCountReg := nextBurstCount
      }
    }

    is(sWriteBurst) {
      wDataReady := true.B
      memWriteAddr := addrReg
      memWriteData := io.wChannel.wData.bits

      when(io.wChannel.wData.valid) {
        val nextWriteAddr = WireDefault(0.U(addrWidth.W))
        val nextBurstCount = WireDefault(0.U(nBursts.W))
        nextWriteAddr := addrReg + 1.U
        nextBurstCount := burstCountReg + 1.U

        memWrEn := true.B

        when(io.wChannel.wLast || burstCountReg === (nBursts - 1).U) {
          stateReg := sIdle
          nextBurstCount := 0.U
          nextWriteAddr := 0.U
        }

        addrReg := nextWriteAddr
        burstCountReg := nextBurstCount
      }
    }
  }

  io.rChannel.rAddr.ready := rAddrReady
  io.rChannel.rData.valid := rDataValid
  io.rChannel.rData.bits := rDataBits
  io.rChannel.rLast := rLast

  io.wChannel.wAddr.ready := wAddrReady
  io.wChannel.wData.ready := wDataReady
}
