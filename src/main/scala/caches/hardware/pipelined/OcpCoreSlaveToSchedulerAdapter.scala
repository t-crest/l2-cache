package caches.hardware.pipelined

import caches.hardware.reppol._
import chisel3._
import chisel3.util._
import ocp._

class OcpCoreSlaveToSchedulerAdapter(nCores: Int, dataWidth: Int) extends Module() {
  val io = IO(new Bundle {
    val core = new OcpCoreSlavePort(log2Up(nCores), dataWidth)
    val scheduler = Flipped(new SchedulerControlIO(nCores, dataWidth))
  })

  val cmd = RegNext(io.core.M.Cmd, 0.U)
  val addr = RegNext(io.core.M.Addr, 0.U)
  val wData = RegNext(io.core.M.Data, 0.U)

  val sResp = WireDefault(OcpResp.NULL)
  val sData = WireDefault(0.U(dataWidth.W))

  when(cmd === OcpCmd.WR || cmd === OcpCmd.RD) {
    sResp := OcpResp.DVA
  }

  val schCmd = WireDefault(SchedulerCmd.NULL)
  switch(cmd) {
    is(OcpCmd.IDLE) {
      schCmd := SchedulerCmd.NULL
    }
    is(OcpCmd.WR) {
      schCmd := SchedulerCmd.WR
    }
    is(OcpCmd.RD) {
      schCmd := SchedulerCmd.RD
    }
  }

  io.scheduler.cmd := schCmd
  io.scheduler.addr := addr
  io.scheduler.wData := wData
  sData := io.scheduler.rData // If the scheduler configuration is memory based then the timing might not be correct

  io.core.S.Resp := sResp
  io.core.S.Data := sData
}
