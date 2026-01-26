package caches.hardware.pipelined

import caches.hardware.reppol._
import chisel3._

/**
 * Top level module for synthesizing the shared pipelined cache.
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

  val io = IO(new OcpCacheWrapperPort(nCores, addrWidth, coreDataWidth, coreBurstLen, memBeatSize * 8, memBurstLen, l2Cache.l2SchedulerDataWidth))

  l2Cache.io.mem <> io.mem
  l2Cache.io.scheduler <> io.scheduler
  l2Cache.io.cores <> io.cores
}

object SharedPipelinedCacheSynthTop extends App {
  // val l2Size = 524288 // 512 KiB
  // val l2Size = 262144 // 256 KiB
  // val l2Size = 131072 // 128 KiB
  val l2Size = 65536 // 64 KiB
  // val l2Size = 16384 // 16 KiB
  val nWays = 8
  val nCores = 2
  val addressWidth = 32
  val bytesPerBlock = 64
  val bytesPerSubBlock = 16
  val memBeatSize = 4
  val memBurstLen = 4

  val l2nSets = l2Size / (nWays * bytesPerBlock)
    val l2RepPolicy = () => new BitPlruReplacementPolicy(nWays, l2nSets, nCores)
  //  val l2RepPolicy = () => new TreePlruReplacementPolicy(nWays, l2nSets, nCores)
  //  val l2RepPolicy = () => new TimeoutReplacementPolicy(nWays, l2nSets, nCores, BasePolicies.BIT_PLRU, repSetFormat = new MruFormat)
//  val l2RepPolicy = () => new ContentionReplacementPolicy(nWays, l2nSets, nCores, BasePolicies.BIT_PLRU, true, true, true, repSetFormat = new MruFormat)

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