package caches.hardware.pipelined

import chisel3._
import chisel3.util._
import ocp._

/**
 * Wrapper for the L2 cache to the OCP burst interface.
 * The adapter asserts write command and the first write data at the same time.
 */
class CacheMemToOcpBurstMasterAdapter(addrWidth: Int, dataWidth: Int, burstLen: Int) extends Module {
  val io = IO(new Bundle {
    val cache = Flipped(new CacheMemoryControllerIO(addrWidth = addrWidth, beatSize = dataWidth))
    val ocpBurst = new OcpBurstMasterPort(addrWidth = addrWidth, dataWidth = dataWidth, burstLen = burstLen)
  })

  val sIdle :: sWriteDelay :: sReadBurst :: sWriteBurst :: sWriteAccept :: Nil = Enum(5)

  // Registers
  val stateReg = RegInit(sIdle)
  val rBurstCountReg = RegInit(0.U(log2Up(burstLen).W))
  val wBurstCountReg = RegInit(0.U(log2Up(burstLen).W))
  val wAddrDelayReg = RegInit(0.U(addrWidth.W))

  val mCmd = WireDefault(OcpCmd.IDLE)
  val mAddr = WireDefault(0.U(addrWidth.W))
  val mDataByteEn = WireDefault(0.U((dataWidth / 8).W))
  val rAddrReady = WireDefault(false.B)
  val wAddrReady = WireDefault(false.B)
  val rLast = WireDefault(false.B)

  switch(stateReg) {
    is(sIdle) {

      when(io.cache.rChannel.rAddr.valid) {
        mCmd := OcpCmd.RD
        mAddr := io.cache.rChannel.rAddr.bits

        when(io.ocpBurst.S.CmdAccept.asBool) {
          rAddrReady := true.B
          stateReg := sReadBurst
        }
      }.elsewhen(io.cache.wChannel.wAddr.valid) {
        wAddrReady := true.B // Need to force the memory interface to enter a state where it provides valid data
        wAddrDelayReg := io.cache.wChannel.wAddr.bits

        stateReg := sWriteDelay
      }
    }

    is(sReadBurst) {
      val nextBurstCount = WireDefault(0.U(log2Up(burstLen).W))
      nextBurstCount := Mux(io.ocpBurst.S.Resp === OcpResp.DVA, rBurstCountReg + 1.U, rBurstCountReg)

      when(rBurstCountReg === (burstLen - 1).U) {
        rLast := true.B
        stateReg := sIdle
        nextBurstCount := 0.U
      }

      rBurstCountReg := nextBurstCount
    }

    is(sWriteDelay) {
      mCmd := OcpCmd.WR
      mAddr := wAddrDelayReg
      mDataByteEn := (math.pow(2, dataWidth / 8) - 1).toInt.U

      when(io.ocpBurst.S.CmdAccept.asBool) {
        stateReg := sWriteBurst
        wBurstCountReg := wBurstCountReg + 1.U
      }
    }

    is(sWriteBurst) {
      val nextBurstCount = WireDefault(0.U(log2Up(burstLen).W))
      nextBurstCount := Mux(io.ocpBurst.S.DataAccept.asBool, wBurstCountReg + 1.U, wBurstCountReg)

      when(wBurstCountReg === (burstLen - 1).U) {
        stateReg := sWriteAccept
        nextBurstCount := 0.U
      }

      wBurstCountReg := nextBurstCount
      mDataByteEn := (math.pow(2, dataWidth / 8) - 1).toInt.U
    }

    is(sWriteAccept) {
      when(io.ocpBurst.S.Resp === OcpResp.DVA) {
        stateReg := sIdle
      }
    }
  }

  io.ocpBurst.M.Cmd := mCmd
  io.ocpBurst.M.Addr := mAddr
  io.ocpBurst.M.Data := io.cache.wChannel.wData.bits
  io.ocpBurst.M.DataByteEn := mDataByteEn
  io.ocpBurst.M.DataValid := io.cache.wChannel.wData.valid

  io.cache.rChannel.rAddr.ready := rAddrReady
  io.cache.rChannel.rData.valid := io.ocpBurst.S.Resp === OcpResp.DVA
  io.cache.rChannel.rData.bits := io.ocpBurst.S.Data
  io.cache.rChannel.rLast := rLast

  io.cache.wChannel.wAddr.ready := wAddrReady
  io.cache.wChannel.wData.ready := io.ocpBurst.S.DataAccept.asBool
}
