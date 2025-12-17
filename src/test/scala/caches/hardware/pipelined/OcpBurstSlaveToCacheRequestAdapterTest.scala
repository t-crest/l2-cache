package caches.hardware.pipelined

import chisel3._
import chiseltest._
import ocp.{OcpCmd, OcpResp}
import org.scalatest.flatspec.AnyFlatSpec

class OcpBurstSlaveToCacheRequestAdapterTest extends AnyFlatSpec with ChiselScalatestTester {
  "OcpBurstSlaveAdapter" should "work" in {
    val addrWidth = 16
    val dataWidth = 32
    val burstLen = 4

    test(new OcpBurstSlaveToCacheRequestAdapter(addrWidth, dataWidth, burstLen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default Assignments
      // ---- Core Port Inputs
      dut.io.corePort.req.reqId.ready.poke(false.B)
      dut.io.corePort.resp.reqId.valid.poke(false.B)
      dut.io.corePort.resp.reqId.bits.poke(0.U)
      dut.io.corePort.resp.rData.poke(0.U)
      // ---- Memory Interface Inputs
      dut.io.ocpBurst.M.Cmd.poke(OcpCmd.IDLE)
      dut.io.ocpBurst.M.Addr.poke(0.U)
      dut.io.ocpBurst.M.Data.poke(0.U)
      dut.io.ocpBurst.M.DataByteEn.poke(0.U)
      dut.io.ocpBurst.M.DataValid.poke(0.U)

      // ------------------ Perform a read request ------------------

      dut.clock.step(1)

      // Issue a read request from the OCP interface
      dut.io.ocpBurst.M.Cmd.poke(OcpCmd.RD)
      dut.io.ocpBurst.M.Addr.poke("h0040".U)

      // Expect the OCP interface to accept the request
      dut.io.ocpBurst.S.CmdAccept.expect(true.B)
      dut.io.ocpBurst.S.Resp.expect(OcpResp.NULL)

      dut.clock.step(1)

      dut.io.ocpBurst.M.Cmd.poke(OcpCmd.IDLE)
      dut.io.ocpBurst.M.Addr.poke(0.U)

      // Expect no response from OCP
      dut.io.ocpBurst.S.CmdAccept.expect(false.B)
      dut.io.ocpBurst.S.Resp.expect(OcpResp.NULL)

      // Expect a valid read request at core port
      dut.io.corePort.req.reqId.valid.expect(true.B)
      dut.io.corePort.req.addr.expect("h0040".U)
      dut.io.corePort.req.rw.expect(false.B)
      dut.io.corePort.req.wData.expect(0.U)
      dut.io.corePort.req.wData.expect(0.U)

      // Accept the request
      dut.io.corePort.req.reqId.ready.poke(true.B)

      dut.clock.step(1)

      dut.io.corePort.req.reqId.ready.poke(false.B)

      dut.clock.step(4)

      // Return a cache response
      dut.io.corePort.resp.reqId.valid.poke(true.B)
      dut.io.corePort.resp.rData.poke("hbadc0ffee0ddf00dbeefbbadbbadbeef".U)

      dut.clock.step(1)

      dut.io.corePort.resp.reqId.valid.poke(false.B)
      dut.io.corePort.resp.rData.poke(0.U)

      // Expect the OCP interface to return the first element of the read data
      dut.io.ocpBurst.S.Data.expect("hbbadbeef".U)
      dut.io.ocpBurst.S.Resp.expect(OcpResp.DVA)

      dut.clock.step(1)

      // Expect the OCP interface to return the second element of the read data
      dut.io.ocpBurst.S.Data.expect("hbeefbbad".U)
      dut.io.ocpBurst.S.Resp.expect(OcpResp.DVA)

      dut.clock.step(1)

      // Expect the OCP interface to return the third element of the read data
      dut.io.ocpBurst.S.Data.expect("he0ddf00d".U)
      dut.io.ocpBurst.S.Resp.expect(OcpResp.DVA)

      dut.clock.step(1)

      // Expect the OCP interface to return the fourth and last element of the read data
      dut.io.ocpBurst.S.Data.expect("hbadc0ffe".U)
      dut.io.ocpBurst.S.Resp.expect(OcpResp.DVA)

      dut.clock.step(1)

      dut.io.ocpBurst.S.Resp.expect(OcpResp.NULL)
      dut.io.ocpBurst.S.Data.expect(0.U)
      dut.io.ocpBurst.S.CmdAccept.expect(0.U)
      dut.io.ocpBurst.S.DataAccept.expect(0.U)

      // ------------------ Perform a write request ------------------
      dut.clock.step(1)

      // Issue a write request to OCP interface
      dut.io.ocpBurst.M.Cmd.poke(OcpCmd.WR)
      dut.io.ocpBurst.M.Addr.poke("h0030".U)
      dut.io.ocpBurst.M.Data.poke("hbeefdead".U)
      dut.io.ocpBurst.M.DataByteEn.poke("b1010".U)
      dut.io.ocpBurst.M.DataValid.poke(true.B)

      // Expect the OCP interface to accept the request
      dut.io.ocpBurst.S.CmdAccept.expect(1.U)
      dut.io.ocpBurst.S.DataAccept.expect(1.U)

      dut.clock.step(1)

      // Send the second element of the write data
      dut.io.ocpBurst.M.Cmd.poke(OcpCmd.IDLE)
      dut.io.ocpBurst.M.Addr.poke(0.U)
      dut.io.ocpBurst.M.Data.poke("hdeadbeef".U)
      dut.io.ocpBurst.M.DataByteEn.poke("b0111".U)
      dut.io.ocpBurst.M.DataValid.poke(true.B)

      // Expect the OCP interface to accept the request
      dut.io.ocpBurst.S.CmdAccept.expect(0.U)
      dut.io.ocpBurst.S.DataAccept.expect(1.U)

      dut.clock.step(1)

      // Send the third element of the write data
      dut.io.ocpBurst.M.Data.poke("hd00dfeed".U)
      dut.io.ocpBurst.M.DataByteEn.poke("b1110".U)
      dut.io.ocpBurst.M.DataValid.poke(true.B)

      // Expect the OCP interface to accept the request
      dut.io.ocpBurst.S.CmdAccept.expect(0.U)
      dut.io.ocpBurst.S.DataAccept.expect(1.U)

      dut.clock.step(1)

      // Send the fourth element of the write data
      dut.io.ocpBurst.M.Data.poke("hcafebabe".U)
      dut.io.ocpBurst.M.DataByteEn.poke("b1111".U)
      dut.io.ocpBurst.M.DataValid.poke(true.B)

      // Expect the OCP interface to accept the request
      dut.io.ocpBurst.S.CmdAccept.expect(0.U)
      dut.io.ocpBurst.S.DataAccept.expect(1.U)

      dut.clock.step(1)

      // End the write transaction at the masters end
      dut.io.ocpBurst.M.Data.poke(0.U)
      dut.io.ocpBurst.M.DataByteEn.poke(0.U)
      dut.io.ocpBurst.M.DataValid.poke(false.B)

      // Expect the OCP interface to accept the request
      dut.io.ocpBurst.S.CmdAccept.expect(0.U)
      dut.io.ocpBurst.S.DataAccept.expect(0.U)

      // Expect the request at the core port
      dut.io.corePort.req.reqId.valid.expect(true.B)
      dut.io.corePort.req.rw.expect(true.B)
      dut.io.corePort.req.wData.expect("hcafebabed00dfeeddeadbeefbeefdead".U)
      dut.io.corePort.req.byteEn.expect("b1111111001111010".U)

      dut.clock.step(1)

      // Accept the WR request at the core port
      dut.io.corePort.req.reqId.ready.poke(true.B)

      dut.clock.step(4)

      // Provide response to the WR request at the core port
      dut.io.corePort.resp.reqId.valid.poke(true.B)

      // Expect the acknowledgement of the OCP WR transaction
      dut.io.ocpBurst.S.Resp.expect(OcpResp.DVA)
      dut.io.ocpBurst.S.Data.expect(0.U)
      dut.io.ocpBurst.S.CmdAccept.expect(0.U)
      dut.io.ocpBurst.S.DataAccept.expect(0.U)

      dut.clock.step(1)
    }
  }
}
