package caches.hardware.pipelined

import caches.hardware.reppol.SchedulerCmd
import caches.hardware.util.Constants.CONTENTION_LIMIT_WIDTH
import chisel3._
import chiseltest._
import ocp.{OcpCmd, OcpResp}
import org.scalatest.flatspec.AnyFlatSpec

class OcpCoreSlaveToSchedulerAdapterTest extends AnyFlatSpec with ChiselScalatestTester {
  "OcpCoreSlaveToSchedulerAdapter" should "adapt OCP core slave signals to scheduler signals" in {
    test(new OcpCoreSlaveToSchedulerAdapter(nCores = 4, dataWidth = CONTENTION_LIMIT_WIDTH)) { dut =>
      // Default assignments
      dut.io.core.M.Cmd.poke(OcpCmd.IDLE)
      dut.io.core.M.Addr.poke(0.U)
      dut.io.core.M.Data.poke(0.U)
      dut.io.core.M.ByteEn.poke(0.U)

      dut.clock.step(1)

      // Issue an OCP write command (set the second core as critical)
      dut.io.core.M.Cmd.poke(OcpCmd.WR)
      dut.io.core.M.Addr.poke(1.U)
      dut.io.core.M.Data.poke(12.U)

      dut.clock.step(1)

      // Check if the scheduler received the correct core ID and command
      dut.io.scheduler.cmd.expect(SchedulerCmd.WR)
      dut.io.scheduler.addr.expect(1.U)
      dut.io.scheduler.wData.expect(12.U)

      // Check if the OCP response is correct
      dut.io.core.S.Resp.expect(OcpResp.DVA)
      dut.io.core.S.Data.expect(0.U)

      // Issue another OCP write command (set the fourth core as critical)
      dut.io.core.M.Cmd.poke(OcpCmd.WR)
      dut.io.core.M.Addr.poke(3.U)
      dut.io.core.M.Data.poke(28.U)

      dut.clock.step(1)

      // Check if the scheduler received the correct core ID and command
      dut.io.scheduler.cmd.expect(SchedulerCmd.WR)
      dut.io.scheduler.addr.expect(3.U)
      dut.io.scheduler.wData.expect(28.U)

      // Check if the OCP response is correct
      dut.io.core.S.Resp.expect(OcpResp.DVA)
      dut.io.core.S.Data.expect(0.U)

      dut.io.core.M.Cmd.poke(OcpCmd.IDLE)
      dut.io.core.M.Addr.poke(0.U)
      dut.io.core.M.Data.poke(0.U)

      dut.clock.step(1)

      // Issue OCP read command (set the second core as non-critical)
      dut.io.core.M.Cmd.poke(OcpCmd.RD)
      dut.io.core.M.Addr.poke(1.U)

      dut.clock.step(1)

      // Check if the scheduler received the correct core ID and command
      dut.io.scheduler.cmd.expect(SchedulerCmd.RD)
      dut.io.scheduler.addr.expect(1.U)
      dut.io.scheduler.wData.expect(0.U)

      // Check if the OCP response is correct
      dut.io.core.S.Resp.expect(OcpResp.DVA)
      dut.io.core.S.Data.expect(0.U)

      // Issue OCP read command (set the second core as non-critical)
      dut.io.core.M.Cmd.poke(OcpCmd.IDLE)
      dut.io.core.M.Addr.poke(0.U)

      dut.clock.step(1)
    }
  }
}
