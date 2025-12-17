package caches.hardware.pipelined

import chisel3._
import chiseltest._
import ocp.{OcpCmd, OcpResp}
import org.scalatest.flatspec.AnyFlatSpec

class CacheMemToOcpBurstMasterAdapterTest extends AnyFlatSpec with ChiselScalatestTester {
  "OcpBurstMasterAdapter" should "work" in {
    val addrWidth = 16
    val dataWidth = 32
    val burstLen = 4

    test(new CacheMemToOcpBurstMasterAdapter(addrWidth, dataWidth, burstLen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default Assignments
      // ---- Memory Interface Inputs
      dut.io.cache.rChannel.rAddr.bits.poke(0.U)
      dut.io.cache.rChannel.rAddr.valid.poke(false.B)
      dut.io.cache.rChannel.rData.ready.poke(false.B)
      dut.io.cache.wChannel.wAddr.bits.poke(0.U)
      dut.io.cache.wChannel.wAddr.valid.poke(false.B)
      dut.io.cache.wChannel.wData.bits.poke(0.U)
      dut.io.cache.wChannel.wData.valid.poke(false.B)
      dut.io.cache.wChannel.wLast.poke(false.B)
      // ---- Memory Interface Inputs
      dut.io.ocpBurst.S.Resp.poke(OcpResp.NULL)
      dut.io.ocpBurst.S.Data.poke(0.U)
      dut.io.ocpBurst.S.CmdAccept.poke(0.U)
      dut.io.ocpBurst.S.DataAccept.poke(0.U)

      // ------------------ Perform a read from the memory interface ------------------
      dut.clock.step(1)

      // Give some read address
      dut.io.cache.rChannel.rAddr.valid.poke(true.B)
      dut.io.cache.rChannel.rAddr.bits.poke("h0040".U)

      // Expect the ocp slave to not be ready yet
      dut.io.cache.rChannel.rAddr.ready.expect(false.B)

      dut.io.ocpBurst.M.Cmd.expect(OcpCmd.RD)
      dut.io.ocpBurst.M.Addr.expect("h0040".U)

      dut.clock.step(1)

      // Make slave accept the command
      dut.io.ocpBurst.S.CmdAccept.poke(1.U)

      // Expect the ocp slave to be ready now
      dut.io.cache.rChannel.rAddr.ready.expect(true.B)

      dut.io.ocpBurst.M.Cmd.expect(OcpCmd.RD)
      dut.io.ocpBurst.M.Addr.expect("h0040".U)

      dut.clock.step(1)

      dut.io.cache.rChannel.rAddr.valid.poke(false.B)
      dut.io.cache.rChannel.rAddr.bits.poke(0.U)
      dut.io.ocpBurst.S.CmdAccept.poke(0.U)

      // Return the first read data element
      dut.io.cache.rChannel.rData.ready.poke(true.B)
      dut.io.ocpBurst.S.Resp.poke(OcpResp.DVA)
      dut.io.ocpBurst.S.Data.poke("hbadc0ffe".U)

      // Assert that the memory interface received the data
      dut.io.cache.rChannel.rData.valid.expect(true.B)
      dut.io.cache.rChannel.rData.bits.expect("hbadc0ffe".U)
      dut.io.ocpBurst.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.ocpBurst.M.Addr.expect(0.U)

      dut.clock.step(1)

      // Return the second read data element
      dut.io.cache.rChannel.rData.ready.poke(true.B)
      dut.io.ocpBurst.S.Resp.poke(OcpResp.DVA)
      dut.io.ocpBurst.S.Data.poke("he0ddf00d".U)

      // Assert that the memory interface received the data
      dut.io.cache.rChannel.rData.valid.expect(true.B)
      dut.io.cache.rChannel.rData.bits.expect("he0ddf00d".U)

      dut.clock.step(1)

      // Return the third read data element
      dut.io.cache.rChannel.rData.ready.poke(true.B)
      dut.io.ocpBurst.S.Resp.poke(OcpResp.DVA)
      dut.io.ocpBurst.S.Data.poke("hbeefbbad".U)

      // Assert that the memory interface received the data
      dut.io.cache.rChannel.rData.valid.expect(true.B)
      dut.io.cache.rChannel.rData.bits.expect("hbeefbbad".U)

      dut.clock.step(1)

      // Return the fourth and last read data element
      dut.io.cache.rChannel.rData.ready.poke(true.B)
      dut.io.ocpBurst.S.Resp.poke(OcpResp.DVA)
      dut.io.ocpBurst.S.Data.poke("hbbadbeef".U)

      // Assert that the memory interface received the data
      dut.io.cache.rChannel.rData.valid.expect(true.B)
      dut.io.cache.rChannel.rData.bits.expect("hbbadbeef".U)
      dut.io.cache.rChannel.rLast.expect(true.B)

      dut.clock.step(1)

      // De-assert the slave signals
      dut.io.cache.rChannel.rData.ready.poke(false.B)
      dut.io.ocpBurst.S.Resp.poke(OcpResp.NULL)
      dut.io.ocpBurst.S.Data.poke(0.U)

      // ------------------ Perform a write from the memory interface ------------------
      dut.clock.step(1)

      // Put up a read address
      dut.io.cache.wChannel.wAddr.valid.poke(true.B)
      dut.io.cache.wChannel.wAddr.bits.poke("h0030".U)

      dut.io.cache.wChannel.wAddr.ready.expect(true.B)

      dut.clock.step(1)

      // De-assert the read address
      dut.io.cache.wChannel.wAddr.valid.poke(false.B)
      dut.io.cache.wChannel.wAddr.bits.poke(0.U)

      dut.io.cache.wChannel.wData.valid.poke(true.B)
      dut.io.cache.wChannel.wData.bits.poke("hbeefdead".U)

      // Expect the slave to not be ready to accept the request yet
      dut.io.cache.wChannel.wData.ready.expect(false.B)

      // Expect the correct request at the OCP end
      dut.io.ocpBurst.M.Addr.expect("h0030".U)
      dut.io.ocpBurst.M.Cmd.expect(OcpCmd.WR)
      dut.io.ocpBurst.M.Data.expect("hbeefdead".U)
      dut.io.ocpBurst.M.DataByteEn.expect("b1111".U)
      dut.io.ocpBurst.M.DataValid.expect(1.U)

      dut.clock.step(1)

      dut.io.cache.wChannel.wData.valid.poke(true.B)
      dut.io.cache.wChannel.wData.bits.poke("hbeefdead".U)

      // Make the slave accept the data and the command
      dut.io.ocpBurst.S.CmdAccept.poke(1.U)
      dut.io.ocpBurst.S.DataAccept.poke(1.U)

      // Expect the slave to be ready to accept the request
      dut.io.cache.wChannel.wData.ready.expect(true.B)

      // Expect the correct request at the OCP end
      dut.io.ocpBurst.M.Addr.expect("h0030".U)
      dut.io.ocpBurst.M.Cmd.expect(OcpCmd.WR)
      dut.io.ocpBurst.M.Data.expect("hbeefdead".U)
      dut.io.ocpBurst.M.DataByteEn.expect("b1111".U)
      dut.io.ocpBurst.M.DataValid.expect(1.U)

      dut.clock.step(1)

      // Give the second element of the write data
      dut.io.cache.wChannel.wData.valid.poke(true.B)
      dut.io.cache.wChannel.wData.bits.poke("hdeadbeef".U)

      // Make the slave accept the data
      dut.io.ocpBurst.S.CmdAccept.poke(0.U)
      dut.io.ocpBurst.S.DataAccept.poke(1.U)

      // Expect the slave to be ready to accept the data
      dut.io.cache.wChannel.wData.ready.expect(true.B)

      // Expect the correct request at the OCP end
      dut.io.ocpBurst.M.Addr.expect(0.U)
      dut.io.ocpBurst.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.ocpBurst.M.Data.expect("hdeadbeef".U)
      dut.io.ocpBurst.M.DataByteEn.expect("b1111".U)
      dut.io.ocpBurst.M.DataValid.expect(1.U)

      dut.clock.step(1)

      // Give the third element of the write data
      dut.io.cache.wChannel.wData.valid.poke(true.B)
      dut.io.cache.wChannel.wData.bits.poke("hd00dfeed".U)

      // Make the slave accept the data
      dut.io.ocpBurst.S.CmdAccept.poke(0.U)
      dut.io.ocpBurst.S.DataAccept.poke(1.U)

      // Expect the slave to be ready to accept the data
      dut.io.cache.wChannel.wData.ready.expect(true.B)

      // Expect the correct request at the OCP end
      dut.io.ocpBurst.M.Addr.expect(0.U)
      dut.io.ocpBurst.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.ocpBurst.M.Data.expect("hd00dfeed".U)
      dut.io.ocpBurst.M.DataByteEn.expect("b1111".U)
      dut.io.ocpBurst.M.DataValid.expect(1.U)

      dut.clock.step(1)

      // Give the fourth element of the write data
      dut.io.cache.wChannel.wData.valid.poke(true.B)
      dut.io.cache.wChannel.wData.bits.poke("hcafebabe".U)
      dut.io.cache.wChannel.wLast.poke(true.B)

      // Make the slave accept the data
      dut.io.ocpBurst.S.CmdAccept.poke(0.U)
      dut.io.ocpBurst.S.DataAccept.poke(1.U)

      // Expect the slave to be ready to accept the data
      dut.io.cache.wChannel.wData.ready.expect(true.B)

      // Expect the correct request at the OCP end
      dut.io.ocpBurst.M.Addr.expect(0.U)
      dut.io.ocpBurst.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.ocpBurst.M.Data.expect("hcafebabe".U)
      dut.io.ocpBurst.M.DataByteEn.expect("b1111".U)
      dut.io.ocpBurst.M.DataValid.expect(1.U)

      dut.clock.step(1)

      // End write transaction
      dut.io.cache.wChannel.wData.valid.poke(false.B)
      dut.io.cache.wChannel.wData.bits.poke(0.U)
      dut.io.cache.wChannel.wLast.poke(false.B)

      // Make the slave accept the data
      dut.io.ocpBurst.S.CmdAccept.poke(0.U)
      dut.io.ocpBurst.S.DataAccept.poke(0.U)
      dut.io.ocpBurst.S.Resp.poke(OcpResp.DVA)

      // Expect the slave to be ready to accept the data
      dut.io.cache.wChannel.wData.ready.expect(false.B)

      // Expect the correct request at the OCP end
      dut.io.ocpBurst.M.Addr.expect(0.U)
      dut.io.ocpBurst.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.ocpBurst.M.Data.expect(0.U)
      dut.io.ocpBurst.M.DataByteEn.expect(0.U)
      dut.io.ocpBurst.M.DataValid.expect(0.U)

      dut.clock.step(1)

      dut.io.ocpBurst.S.Resp.poke(OcpResp.NULL)

      // Expect the outputs to go to default
      dut.io.ocpBurst.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.ocpBurst.M.Addr.expect(0.U)
      dut.io.ocpBurst.M.Data.expect(0.U)
      dut.io.ocpBurst.M.DataByteEn.expect(0.U)
      dut.io.ocpBurst.M.DataValid.expect(0.U)

      dut.io.cache.rChannel.rAddr.ready.expect(false.B)
      dut.io.cache.rChannel.rData.valid.expect(false.B)
      dut.io.cache.rChannel.rData.bits.expect(0.U)
      dut.io.cache.rChannel.rLast.expect(false.B)

      dut.io.cache.wChannel.wAddr.ready.expect(false.B)
      dut.io.cache.wChannel.wData.ready.expect(false.B)
    }
  }
}
