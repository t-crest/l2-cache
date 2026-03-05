package caches.hardware.pipelined

import caches.hardware.reppol._
import chisel3._
import chisel3.util._

/**
 * Top level module for synthesizing the shared pipelined cache.
 * The top level module uses shift registers to serialize input and combines the cache's output signals
 * in to a single signal, in order to reduce the number of top-level pins while preventing the synthesis tool from
 * synthesizing away internal logic.
 */
class SharedPipelinedCacheSynthTop(
                                    sizeInBytes: Int,
                                    nWays: Int,
                                    nCores: Int,
                                    addrWidth: Int,
                                    bytesPerBlock: Int,
                                    bytesPerSubBlock: Int,
                                    memBeatSize: Int,
                                    memBurstLen: Int,
                                    l2RepPolicyGen: () => SharedCacheReplacementPolicyType,
                                    nMshrs: Option[Int] = None,
                                    nHalfMissCmds: Option[Int] = None,
                                  ) extends Module {
  private val coreDataWidth = 32
  private val memDataWidth = memBeatSize * 8
  private val coreByteEnWidth = coreDataWidth / 8
  private val memByteEnWidth = memDataWidth / 8
  private val coreBurstLen = bytesPerSubBlock / (coreDataWidth / 8)

  val l2CacheGen = () => new SharedPipelinedCache(
    sizeInBytes = sizeInBytes,
    nWays = nWays,
    nCores = nCores,
    reqIdWidth = 1,
    addressWidth = addrWidth,
    bytesPerBlock = bytesPerBlock,
    bytesPerSubBlock = bytesPerSubBlock,
    memBeatSize = memBeatSize,
    memBurstLen = memBurstLen,
    l2RepPolicy = l2RepPolicyGen,
    nMshrs = nMshrs,
    nHalfMissCmds = nHalfMissCmds
  )

  val l2Cache = Module(new OcpCacheWrapper(
    nCores = nCores,
    addrWidth = addrWidth,
    coreDataWidth = coreDataWidth,
    coreBurstLen = coreBurstLen,
    memDataWidth = memBeatSize * 8,
    memBurstLen = memBurstLen,
    l2Cache = l2CacheGen
  ))

  private val schedulerAddrWidth = log2Up(nCores)
  private val schedulerDataWidth = l2Cache.l2SchedulerDataWidth
  private val schedulerByteEnWidth = schedulerDataWidth / 8

  val io = IO(new Bundle {
    val serialInCores = Input(UInt(1.W))
    val serialInMem = Input(UInt(1.W))
    val serialInScheduler = Input(UInt(1.W))
    val reducedOut = Output(UInt(1.W))
  })

  // Input shift registers
  private val coreInShiftRegisterWidth = 3 + addrWidth + coreDataWidth + coreByteEnWidth + 1
  val coresInShiftReg = Reg(UInt((nCores * coreInShiftRegisterWidth).W))
  val memInShiftReg = Reg(UInt((2 + memDataWidth + 1 + 1).W))
  val schedulerInShiftReg = Reg(UInt((3 + schedulerAddrWidth + schedulerDataWidth + schedulerByteEnWidth).W))

  // Output registers
  val coresOutReg = Reg(UInt((nCores * (2 + coreDataWidth + 1 + 1)).W))
  val memOutReg = Reg(UInt((3 + addrWidth + memDataWidth + memByteEnWidth + 1).W))
  val schedulerOutReg = Reg(UInt((2 + schedulerDataWidth).W))

  // Provide input serially through shift registers to reduce number of input pins for synthesis
  coresInShiftReg := Cat(coresInShiftReg(coresInShiftReg.getWidth - 2, 0), io.serialInCores)
  memInShiftReg := Cat(memInShiftReg(memInShiftReg.getWidth - 2, 0), io.serialInMem)
  schedulerInShiftReg := Cat(schedulerInShiftReg(schedulerInShiftReg.getWidth - 2, 0), io.serialInScheduler)

  // Core interfaces
  val coresOut = VecInit(Seq.fill(nCores)(WireDefault(0.U((2 + coreDataWidth + 1 + 1).W))))
  for (core <- 0 until nCores) {
    val coreOffset = core * coreInShiftRegisterWidth

    l2Cache.io.cores(core).M.Cmd := coresInShiftReg(3 + addrWidth + coreDataWidth + coreByteEnWidth + coreOffset, addrWidth + coreDataWidth + coreByteEnWidth + 1 + coreOffset)
    l2Cache.io.cores(core).M.Addr := coresInShiftReg(addrWidth + coreDataWidth + coreByteEnWidth + coreOffset, coreDataWidth + coreByteEnWidth + 1 + coreOffset)
    l2Cache.io.cores(core).M.Data := coresInShiftReg(coreDataWidth + coreByteEnWidth + coreOffset, coreByteEnWidth + 1 + coreOffset)
    l2Cache.io.cores(core).M.DataByteEn := coresInShiftReg(coreByteEnWidth + coreOffset, 1 + coreOffset)
    l2Cache.io.cores(core).M.DataValid := coresInShiftReg(coreOffset)

    coresOut(core) := Cat(l2Cache.io.cores(core).S.Resp, l2Cache.io.cores(core).S.Data, l2Cache.io.cores(core).S.CmdAccept, l2Cache.io.cores(core).S.DataAccept)
  }

  coresOutReg := Cat(coresOut)

  // Memory interface
  l2Cache.io.mem.S.Resp := memInShiftReg((2 + memDataWidth + 2) - 1, memDataWidth + 2)
  l2Cache.io.mem.S.Data := memInShiftReg((memDataWidth + 2) - 1, 2)
  l2Cache.io.mem.S.CmdAccept := memInShiftReg(1)
  l2Cache.io.mem.S.DataAccept := memInShiftReg(0)

  memOutReg := Cat(l2Cache.io.mem.M.Cmd, l2Cache.io.mem.M.Addr, l2Cache.io.mem.M.Data, l2Cache.io.mem.M.DataByteEn, l2Cache.io.mem.M.DataValid)

  // Scheduler interface
  l2Cache.io.scheduler.M.Cmd := schedulerInShiftReg((3 + schedulerAddrWidth + schedulerDataWidth + schedulerByteEnWidth) - 1, schedulerAddrWidth + schedulerDataWidth + schedulerByteEnWidth)
  l2Cache.io.scheduler.M.Addr := schedulerInShiftReg((schedulerAddrWidth + schedulerDataWidth + schedulerByteEnWidth) - 1, schedulerDataWidth + schedulerByteEnWidth)
  l2Cache.io.scheduler.M.Data := schedulerInShiftReg((schedulerDataWidth + schedulerByteEnWidth) - 1, schedulerByteEnWidth)
  l2Cache.io.scheduler.M.ByteEn := schedulerInShiftReg(schedulerByteEnWidth - 1, 0)

  schedulerOutReg := Cat(l2Cache.io.scheduler.S.Resp, l2Cache.io.scheduler.S.Data)

  // Combine all the retimed output signals using an XOR operation so the synthesis tool doesn't remove logic
  io.reducedOut := RegNext(RegNext(Cat(coresOutReg, memOutReg, schedulerOutReg).xorR))
}

object SharedPipelinedCacheSynthTop extends App {
  val l2Size = 65536 // 64 KiB
  val nWays = 8
  val nCores = 4
  val addressWidth = 32
  val bytesPerBlock = 64
  val bytesPerSubBlock = 16
  val memBeatSize = 4
  val memBurstLen = 4

  val l2nSets = l2Size / (nWays * bytesPerBlock)
  //  val l2RepPolicy = () => new BitPlruReplacementPolicy(nWays, l2nSets, nCores)
  val l2RepPolicy = () => new ContentionReplacementPolicy(nWays, l2nSets, nCores, BasePolicies.BIT_PLRU, true, true, true, repSetFormat = new MruFormat)

  println("Generating the L2 cache hardware for synthesis...")
  (new chisel3.stage.ChiselStage).emitVerilog(
    new SharedPipelinedCacheSynthTop(
      sizeInBytes = l2Size,
      nWays = nWays,
      nCores = nCores,
      addrWidth = addressWidth,
      bytesPerBlock = bytesPerBlock,
      bytesPerSubBlock = bytesPerSubBlock,
      memBeatSize = memBeatSize,
      memBurstLen = memBurstLen,
      l2RepPolicyGen = l2RepPolicy
    ),
    Array("--target-dir", "generated")
  )
}