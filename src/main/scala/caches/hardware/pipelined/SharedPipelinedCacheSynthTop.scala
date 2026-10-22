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
  val schedulerInShiftReg = Reg(UInt((3 + schedulerAddrWidth + schedulerDataWidth).W))

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
  l2Cache.io.scheduler.M.Cmd := schedulerInShiftReg((3 + schedulerAddrWidth + schedulerDataWidth) - 1, schedulerAddrWidth + schedulerDataWidth)
  l2Cache.io.scheduler.M.Addr := schedulerInShiftReg((schedulerAddrWidth + schedulerDataWidth) - 1, schedulerDataWidth)
  l2Cache.io.scheduler.M.Data := schedulerInShiftReg(schedulerDataWidth - 1, 0)
  l2Cache.io.scheduler.M.ByteEn := 0.U // This signal is not used by the cache, but only defined by the interface

  schedulerOutReg := Cat(l2Cache.io.scheduler.S.Resp, l2Cache.io.scheduler.S.Data)

  // Combine all the retimed output signals using an XOR operation so the synthesis tool doesn't remove logic
  io.reducedOut := RegNext(RegNext(Cat(coresOutReg, memOutReg, schedulerOutReg).xorR))
}

/**
 * Generates the synthesis top level module.
 *
 * Arguments: [policy] [nCores]
 *  - policy: L2 replacement policy, configured like in Patmos (repl attribute of the L2Cache element in the Patmos
 *            configuration): plru, cont, time, lc, lcr, part, lcq or partq (default: time)
 *  - nCores: number of cores sharing the cache (default: 8)
 */
object SharedPipelinedCacheSynthTop extends App {
  val l2Size = 65536 // 64 KiB
  val nWays = 8
  val nCores = if (args.length > 1) args(1).toInt else 8
  val addressWidth = 32
  val bytesPerBlock = 64
  val bytesPerSubBlock = 16
  val memBeatSize = 4
  val memBurstLen = 4

  val l2nSets = l2Size / (nWays * bytesPerBlock)
  val policy = if (args.length > 0) args(0) else "time"
  val l2RepPolicy: () => SharedCacheReplacementPolicyType = policy match {
    case "plru" => () => new BitPlruReplacementPolicy(nWays, l2nSets, nCores)
    case "cont" => () => new ContentionReplacementPolicy(nWays, l2nSets, nCores, BasePolicies.BIT_PLRU, enablePrecedentEvents = true, enableWbEvents = true, enableMissInMiss = true)
    case "time" => () => new TimeoutReplacementPolicy(nWays, l2nSets, nCores, BasePolicies.BIT_PLRU, repSetFormat = new MruFormat)
    case "lc" => () => new LeastCriticalReplacementPolicy(nWays, l2nSets, nCores, BasePolicies.BIT_PLRU)
    case "lcr" => () => new LeastCriticalReplacementPolicy(nWays, l2nSets, nCores, BasePolicies.BIT_PLRU, rejectNonCritical = true)
    case "part" => () => new CriticalWayPartitionReplacementPolicy(nWays, l2nSets, nCores)
    case "lcq" => () => new LeastCriticalReplacementPolicy(nWays, l2nSets, nCores, BasePolicies.BIT_PLRU, priorityQueues = true)
    case "partq" => () => new CriticalWayPartitionReplacementPolicy(nWays, l2nSets, nCores, priorityQueues = true)
    case _ => throw new IllegalArgumentException(s"Unknown L2 cache replacement policy: $policy")
  }

  println(s"Generating the L2 cache hardware for synthesis with the $policy replacement policy and $nCores cores...")
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
