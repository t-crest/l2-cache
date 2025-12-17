package caches.hardware.pipelined

import chisel3._
import chisel3.util._
import ocp._

class OcpBurstSlaveToCacheRequestAdapter(addrWidth: Int, dataWidth: Int, burstLen: Int) extends Module {
  val io = IO(new Bundle {
    val ocpBurst = new OcpBurstSlavePort(addrWidth, dataWidth, burstLen)
    val corePort = Flipped(new CacheCorePortIO(addrWidth, dataWidth * burstLen, reqIdWidth = 1))
  })

  // We first latch the request, then we issue it in the next clock cycle and wait for it to be accepted,
  // once accepted we wait for response
  val sIdle :: sWaitReadAccept :: sWaitReadResp :: sReadBurst :: sWriteBurst :: sWaitWriteAccept :: sWaitWriteResp :: Nil = Enum(7)

  // Registers
  val stateReg = RegInit(sIdle)
  val addrReg = RegInit(0.U(addrWidth.W))
  val dataCountReg = RegInit(0.U(log2Up(burstLen).W))
  val dataRegs = RegInit(VecInit(Seq.fill(burstLen)(0.U(dataWidth.W))))
  val dateByteEnRegs = RegInit(VecInit(Seq.fill(burstLen)(0.U((dataWidth / 8).W))))

  // Default signal assignments
  val ocpSResp = WireDefault(OcpResp.NULL)
  val ocpSData = WireDefault(0.U(dataWidth.W))
  val ocpSCmdAccept = WireDefault(0.U(1.W))
  val ocpSDataAccept = WireDefault(0.U(1.W))

  val cacheReqValid = WireDefault(false.B)
  val cacheReqRw = WireDefault(false.B)
  val cacheReqWData = WireDefault(0.U((dataWidth * burstLen).W))
  val cacheReqByteEn = WireDefault(0.U(((dataWidth * burstLen) / 8).W))

  switch(stateReg) {
    is(sIdle) {

      when(io.ocpBurst.M.Cmd === OcpCmd.RD) {
        ocpSCmdAccept := 1.U
        addrReg := io.ocpBurst.M.Addr
        stateReg := sWaitReadAccept
      }.elsewhen(io.ocpBurst.M.Cmd === OcpCmd.WR) {
        // Need to latch rest of the data over multiple cycles
        ocpSDataAccept := 1.U
        ocpSCmdAccept := 1.U
        dataCountReg := dataCountReg + 1.U
        dataRegs(0) := io.ocpBurst.M.Data
        dateByteEnRegs(0) := io.ocpBurst.M.DataByteEn
        addrReg := io.ocpBurst.M.Addr
        stateReg := sWriteBurst
      }
    }

    is(sWaitReadAccept) {
      cacheReqValid := true.B

      when(io.corePort.req.reqId.ready) {
        stateReg := sWaitReadResp
      }
    }

    is(sWaitReadResp) {
      when(io.corePort.resp.reqId.valid) {
        // Need to assign the read data to the registers
        for (i <- 0 until dataRegs.length) {
          dataRegs(i) := io.corePort.resp.rData((dataWidth - 1) + (i * dataWidth), i * dataWidth)
        }

        stateReg := sReadBurst
      }
    }

    is(sReadBurst) {
      val nextDataCount = WireDefault(0.U(log2Up(burstLen).W))
      nextDataCount := dataCountReg + 1.U

      when(dataCountReg === (burstLen - 1).U) {
        stateReg := sIdle
        nextDataCount := 0.U
      }

      ocpSResp := OcpResp.DVA
      ocpSData := dataRegs(dataCountReg)
      dataCountReg := nextDataCount
    }

    is(sWriteBurst) {
      val nextDataCount = WireDefault(0.U(log2Up(burstLen).W))

      when(io.ocpBurst.M.DataValid.asBool) {
        dataRegs(dataCountReg) := io.ocpBurst.M.Data
        dateByteEnRegs(dataCountReg) := io.ocpBurst.M.DataByteEn
        ocpSDataAccept := true.B
        nextDataCount := dataCountReg + 1.U
      }

      when(dataCountReg === (burstLen - 1).U) {
        stateReg := sWaitWriteAccept
        nextDataCount := 0.U
      }

      dataCountReg := nextDataCount
    }

    is(sWaitWriteAccept) {
      cacheReqValid := true.B
      cacheReqWData := dataRegs.asUInt
      cacheReqByteEn := dateByteEnRegs.asUInt
      cacheReqRw := true.B

      when(io.corePort.req.reqId.ready) {
        stateReg := sWaitWriteResp
      }
    }

    is(sWaitWriteResp) {
      when(io.corePort.resp.reqId.valid) {
        ocpSResp := OcpResp.DVA
        stateReg := sIdle
      }
    }
  }

  io.corePort.req.reqId.valid := cacheReqValid
  io.corePort.req.reqId.bits := 0.U // Since OCP does not provide request IDs, we set reqID to static value
  io.corePort.req.addr := addrReg
  io.corePort.req.rw := cacheReqRw
  io.corePort.req.byteEn := cacheReqByteEn
  io.corePort.req.wData := cacheReqWData

  io.ocpBurst.S.Resp := ocpSResp
  io.ocpBurst.S.Data := ocpSData
  io.ocpBurst.S.CmdAccept := ocpSCmdAccept
  io.ocpBurst.S.DataAccept := ocpSDataAccept
}
