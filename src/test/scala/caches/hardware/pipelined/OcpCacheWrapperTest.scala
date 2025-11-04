package caches.hardware.pipelined

import ocp.{OcpCmd, OcpResp}
import caches.hardware.reppol.{BasePolicies, ContentionReplacementPolicy}
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class OcpCacheWrapperTest extends AnyFlatSpec with ChiselScalatestTester {
  "OcpCacheWrapper" should "accept OCP burst commands and issue OCP burst commands to external memory" in {
    val nCores = 4
    val addrWidth = 32
    val coreDataWidth = 32
    val coreBurstLen = 4
    val memDataWidth = 32
    val memBurstLen = 4

    val l2Size = 1024
    val nWays = 8
    val bytesPerBlock = 32
    val l2Sets = l2Size / (nWays * bytesPerBlock)

    val l2ContPolGen = () => new ContentionReplacementPolicy(nWays, l2Sets, nCores, BasePolicies.BIT_PLRU)

    val l2CacheGen = () => new SharedPipelinedCache(
      sizeInBytes = l2Size,
      nWays = 8,
      nCores = nCores,
      reqIdWidth = 1,
      addressWidth = addrWidth,
      bytesPerBlock = bytesPerBlock,
      bytesPerSubBlock = 16,
      memBeatSize = memDataWidth / 8,
      memBurstLen = memBurstLen,
      l2RepPolicy = l2ContPolGen
    )

    test(new OcpCacheWrapper(
      nCores = nCores,
      addrWidth = addrWidth,
      coreDataWidth = coreDataWidth,
      coreBurstLen = coreBurstLen,
      memDataWidth = memDataWidth,
      memBurstLen = memBurstLen,
      l2Cache = l2CacheGen
    )).withAnnotations(Seq(WriteFstAnnotation, VerilatorBackendAnnotation)) { dut =>
      // Default assignments
      for (i <- 0 until nCores) {
        dut.io.cores(i).M.Cmd.poke(OcpCmd.IDLE)
        dut.io.cores(i).M.Addr.poke(0.U)
        dut.io.cores(i).M.Data.poke(0.U)
        dut.io.cores(i).M.DataByteEn.poke(0.U)
        dut.io.cores(i).M.DataValid.poke(0.U)
      }

      dut.io.mem.S.Resp.poke(OcpResp.NULL)
      dut.io.mem.S.Data.poke(0.U)
      dut.io.mem.S.CmdAccept.poke(0.U)
      dut.io.mem.S.DataAccept.poke(0.U)

      dut.io.scheduler.M.Cmd.poke(OcpCmd.IDLE)
      dut.io.scheduler.M.Addr.poke(0.U)
      dut.io.scheduler.M.Data.poke(0.U)

      dut.clock.step(5)

      // --------------- Set the first core as critical ---------------
      dut.io.scheduler.M.Cmd.poke(OcpCmd.WR)
      dut.io.scheduler.M.Addr.poke(1.U)
      dut.io.scheduler.M.Data.poke(2.U)

      dut.clock.step(1)

      dut.io.scheduler.M.Cmd.poke(OcpCmd.IDLE)
      dut.io.scheduler.M.Addr.poke(0.U)
      dut.io.scheduler.M.Data.poke(0.U)

      dut.io.scheduler.S.Resp.expect(OcpResp.DVA)

      dut.clock.step(1)

      // --------------- Issue a write command to the cache from first core ---------------
      dut.io.cores(0).M.Cmd.poke(OcpCmd.WR)
      dut.io.cores(0).M.Addr.poke("h08f0".U)
      dut.io.cores(0).M.Data.poke("hdeadbeef".U) // Give the first data word
      dut.io.cores(0).M.DataByteEn.poke("b1110".U)
      dut.io.cores(0).M.DataValid.poke(1.U)

      dut.io.cores(0).S.Resp.expect(OcpResp.NULL)
      dut.io.cores(0).S.Data.expect(0.U)
      dut.io.cores(0).S.CmdAccept.expect(true.B)
      dut.io.cores(0).S.DataAccept.expect(true.B)

      dut.clock.step(1)

      dut.io.cores(0).M.Cmd.poke(OcpCmd.IDLE)
      dut.io.cores(0).M.Addr.poke(0.U)
      dut.io.cores(0).M.Data.poke("hcafebabe".U) // Give the second data word
      dut.io.cores(0).M.DataByteEn.poke("b0110".U)
      dut.io.cores(0).M.DataValid.poke(1.U)

      dut.io.cores(0).S.Resp.expect(OcpResp.NULL)
      dut.io.cores(0).S.Data.expect(0.U)
      dut.io.cores(0).S.CmdAccept.expect(false.B)
      dut.io.cores(0).S.DataAccept.expect(true.B)

      dut.clock.step(1)

      dut.io.cores(0).M.Data.poke("hbeefdead".U) // Give the third data word
      dut.io.cores(0).M.DataByteEn.poke("b1111".U)
      dut.io.cores(0).M.DataValid.poke(1.U)

      dut.io.cores(0).S.Resp.expect(OcpResp.NULL)
      dut.io.cores(0).S.Data.expect(0.U)
      dut.io.cores(0).S.CmdAccept.expect(false.B)
      dut.io.cores(0).S.DataAccept.expect(true.B)

      dut.clock.step(1)

      dut.io.cores(0).M.Data.poke("hbabecafe".U) // Give the fourth data word
      dut.io.cores(0).M.DataByteEn.poke("b1001".U)
      dut.io.cores(0).M.DataValid.poke(1.U)

      dut.io.cores(0).S.Resp.expect(OcpResp.NULL)
      dut.io.cores(0).S.Data.expect(0.U)
      dut.io.cores(0).S.CmdAccept.expect(false.B)
      dut.io.cores(0).S.DataAccept.expect(true.B)

      dut.clock.step(1)

      dut.io.cores(0).M.Data.poke(0.U)
      dut.io.cores(0).M.DataValid.poke(0.U)

      dut.io.cores(0).S.Resp.expect(OcpResp.NULL) // Expect the write data to not be acknowledged yet
      dut.io.cores(0).S.Data.expect(0.U)
      dut.io.cores(0).S.CmdAccept.expect(false.B)
      dut.io.cores(0).S.DataAccept.expect(false.B)

      dut.clock.step(6)

      // --------------- Expect the cache to issue a read command ---------------
      dut.io.mem.M.Cmd.expect(OcpCmd.RD)
      dut.io.mem.M.Addr.expect("h08e0".U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      // Simulate the slave not being ready
      dut.clock.step(2)

      dut.io.mem.M.Cmd.expect(OcpCmd.RD)
      dut.io.mem.M.Addr.expect("h08e0".U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      // Simulate the slave being ready now
      dut.io.mem.S.Resp.poke(OcpResp.NULL)
      dut.io.mem.S.CmdAccept.poke(1.U)

      dut.clock.step(1)

      dut.io.mem.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.mem.M.Addr.expect(0.U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      // Simulate the slave being ready now
      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("h00df00df".U)
      dut.io.mem.S.CmdAccept.poke(0.U)

      dut.clock.step(1)

      dut.io.mem.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.mem.M.Addr.expect(0.U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("hbadc0ffe".U)

      dut.clock.step(1)

      dut.io.mem.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.mem.M.Addr.expect(0.U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("hd15ea5ef".U)

      dut.clock.step(1)

      dut.io.mem.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.mem.M.Addr.expect(0.U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("hbaddbeef".U)

      dut.clock.step(1)

      // --------------- Expect the cache to issue a second read command ---------------
      dut.io.mem.M.Cmd.expect(OcpCmd.RD)
      dut.io.mem.M.Addr.expect("h08f0".U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      // Simulate the slave being ready now
      dut.io.mem.S.Resp.poke(OcpResp.NULL)
      dut.io.mem.S.CmdAccept.poke(1.U)

      dut.clock.step(1)

      dut.io.mem.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.mem.M.Addr.expect(0.U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("hbadf00df".U)
      dut.io.mem.S.CmdAccept.poke(0.U)

      dut.clock.step(1)

      dut.io.mem.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.mem.M.Addr.expect(0.U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      // Simulate the slave being ready now
      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("h600dcafe".U)

      dut.clock.step(1)

      dut.io.mem.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.mem.M.Addr.expect(0.U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      // Simulate the slave being ready now
      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("hfaceb00c".U)

      dut.clock.step(1)

      dut.io.mem.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.mem.M.Addr.expect(0.U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      // Simulate the slave being ready now
      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("hb00cb00c".U)

      dut.clock.step(1)

      dut.io.mem.S.Resp.poke(OcpResp.NULL)
      dut.io.mem.S.Data.poke(0.U)

      dut.clock.step(2)

      dut.io.cores(0).S.Resp.expect(OcpResp.DVA) // Expect the write data to not be acknowledged after it was brought to the cache
      dut.io.cores(0).S.Data.expect(0.U)
      dut.io.cores(0).S.CmdAccept.expect(false.B)
      dut.io.cores(0).S.DataAccept.expect(false.B)

      dut.clock.step(1)

      // --------------- Issue a read command to the cache from third core ---------------
      dut.io.cores(2).M.Cmd.poke(OcpCmd.RD)
      dut.io.cores(2).M.Addr.poke("ha4f7".U)

      dut.io.cores(2).S.Resp.expect(OcpResp.NULL)
      dut.io.cores(2).S.Data.expect(0.U)
      dut.io.cores(2).S.CmdAccept.expect(true.B)
      dut.io.cores(2).S.DataAccept.expect(false.B)

      dut.clock.step(1)

      dut.io.cores(2).M.Cmd.poke(OcpCmd.IDLE)
      dut.io.cores(2).M.Addr.poke(0.U)

      // Step until the cache generates a read request to memory
      dut.clock.step(6)

      dut.io.mem.M.Cmd.expect(OcpCmd.RD)
      dut.io.mem.M.Addr.expect("ha4e0".U)

      dut.io.mem.S.Resp.poke(OcpResp.NULL)
      dut.io.mem.S.Data.poke(0.U)
      dut.io.mem.S.CmdAccept.poke(true.B)
      dut.io.mem.S.DataAccept.poke(false.B)

      dut.clock.step(1)

      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("hfaceb00c".U) // Give the first data word

      dut.clock.step(1)

      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("h00d15ea5".U) // Give the second data word

      dut.clock.step(1)

      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("hc0010ff0".U) // Give the second data word

      dut.clock.step(1)

      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("hc001d00d".U) // Give the second data word

      dut.clock.step(1)

      dut.io.mem.S.Resp.poke(OcpResp.NULL)
      dut.io.mem.S.Data.poke(0.U) // Give the second data word

      // Expect the cache to generate a second read request to memory
      dut.io.mem.M.Cmd.expect(OcpCmd.RD)
      dut.io.mem.M.Addr.expect("ha4f0".U)

      dut.io.mem.S.Resp.poke(OcpResp.NULL)
      dut.io.mem.S.Data.poke(0.U)
      dut.io.mem.S.CmdAccept.poke(true.B)
      dut.io.mem.S.DataAccept.poke(false.B)

      dut.clock.step(1)

      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("h542af425".U) // Give the first data word

      dut.clock.step(1)

      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("hb000000d".U) // Give the second data word

      dut.clock.step(1)

      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("hff5e3a41".U) // Give the second data word

      dut.clock.step(1)

      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("h6abb4cee".U) // Give the second data word

      dut.clock.step(1)

      dut.io.mem.S.Resp.poke(OcpResp.NULL)
      dut.io.mem.S.Data.poke(0.U) // Give the second data word

      // Expect the cache to return the read data to the core
      dut.clock.step(3)

      dut.io.cores(2).S.Resp.expect(OcpResp.DVA)
      dut.io.cores(2).S.Data.expect("h542af425".U)
      dut.io.cores(2).S.CmdAccept.expect(false.B)
      dut.io.cores(2).S.DataAccept.expect(false.B)

      dut.clock.step(1)

      dut.io.cores(2).S.Resp.expect(OcpResp.DVA)
      dut.io.cores(2).S.Data.expect("hb000000d".U)
      dut.io.cores(2).S.CmdAccept.expect(false.B)
      dut.io.cores(2).S.DataAccept.expect(false.B)

      dut.clock.step(1)

      dut.io.cores(2).S.Resp.expect(OcpResp.DVA)
      dut.io.cores(2).S.Data.expect("hff5e3a41".U)
      dut.io.cores(2).S.CmdAccept.expect(false.B)
      dut.io.cores(2).S.DataAccept.expect(false.B)

      dut.clock.step(1)

      dut.io.cores(2).S.Resp.expect(OcpResp.DVA)
      dut.io.cores(2).S.Data.expect("h6abb4cee".U)
      dut.io.cores(2).S.CmdAccept.expect(false.B)
      dut.io.cores(2).S.DataAccept.expect(false.B)

      dut.clock.step(1)
    }
  }
}
