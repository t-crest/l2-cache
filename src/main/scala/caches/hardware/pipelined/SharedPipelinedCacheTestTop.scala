package caches.hardware.pipelined

import chisel3._
import chisel3.util._
import caches.hardware.reppol._
import caches.hardware.util.DummyDRAM

class SharedCachePort(nCores: Int, reqIdWidth: Int, addrWidth: Int, dataWidth: Int) extends Bundle {
  val cores = Vec(nCores, new CacheCorePortIO(addrWidth, dataWidth, reqIdWidth))
}

/**
 * Top level module for testing the shared pipelined cache.
 * Contains a request arbiter, the cache itself, and a dummy memory.
 */
class SharedPipelinedCacheTestTop(
                                   sizeInBytes: Int,
                                   nWays: Int,
                                   nCores: Int,
                                   reqIdWidth: Int,
                                   addressWidth: Int,
                                   bytesPerBlock: Int,
                                   bytesPerSubBlock: Int,
                                   memBeatSize: Int,
                                   memBurstLen: Int,
                                   l2RepPolicyGen: () => SharedCacheReplacementPolicyType,
                                   nMshrs: Option[Int] = None,
                                   nHalfMissCmds: Option[Int] = None,
                                   dataFile: Option[String] = None,
                                   printCacheInfo: Boolean = true
                                 ) extends Module {
  require(isPow2(memBeatSize), "Bytes per burst need to be a power of 2.")

  val subBlockDataWidth = bytesPerSubBlock * 8
  val arbiter = Module(new RoundRobinRequestArbiter(nCores, addressWidth, bytesPerSubBlock * 8, reqIdWidth))

  val l2Cache = Module(new SharedPipelinedCache(
    sizeInBytes = sizeInBytes,
    nWays = nWays,
    nCores = nCores,
    reqIdWidth = reqIdWidth,
    addressWidth = addressWidth,
    bytesPerBlock = bytesPerBlock,
    bytesPerSubBlock = bytesPerSubBlock,
    memBeatSize = memBeatSize,
    memBurstLen = memBurstLen,
    l2RepPolicy = l2RepPolicyGen,
    nMshrs = nMshrs,
    nHalfMissCmds = nHalfMissCmds,
    printInfo = printCacheInfo
  ))

  val io = IO(new Bundle {
    val scheduler = new SchedulerControlIO(nCores, l2Cache.schedulerDataWidth)
    val requests = new SharedCachePort(nCores, reqIdWidth: Int, addressWidth, bytesPerSubBlock * 8)
  })

  // The dummy memory is sub-block addressable
  val memory = Module(new DummyDRAM(addressWidth - log2Ceil(memBeatSize), bytesPerBlock * 8, memBeatSize, memBurstLen, dataFile))

  for (coreIdx <- 0 until nCores) {
    arbiter.io.ports(coreIdx).reqId <> io.requests.cores(coreIdx).req.reqId
    arbiter.io.ports(coreIdx).addr := io.requests.cores(coreIdx).req.addr
    arbiter.io.ports(coreIdx).rw := io.requests.cores(coreIdx).req.rw
    arbiter.io.ports(coreIdx).byteEn := io.requests.cores(coreIdx).req.byteEn
    arbiter.io.ports(coreIdx).wData := io.requests.cores(coreIdx).req.wData
  }

  l2Cache.io.scheduler <> io.scheduler

  // Connection between request arbiter and the cache
  l2Cache.io.core.req.reqId <> arbiter.io.out.reqId
  l2Cache.io.core.req.rw <> arbiter.io.out.rw
  l2Cache.io.core.req.addr <> arbiter.io.out.addr
  l2Cache.io.core.req.wData <> arbiter.io.out.wData
  l2Cache.io.core.req.byteEn <> arbiter.io.out.byteEn
  l2Cache.io.inCoreId := arbiter.io.chosen

  // Connection between dummy memory and the cache
  // Memory read address channel
  memory.io.rChannel.rAddr.valid := l2Cache.io.mem.rChannel.rAddr.valid
  memory.io.rChannel.rAddr.bits := l2Cache.io.mem.rChannel.rAddr.bits(addressWidth - 1, log2Ceil(memBeatSize)) // Dummy memory accepts the address bits for bursts addresses only
  l2Cache.io.mem.rChannel.rAddr.ready := memory.io.rChannel.rAddr.ready

  // Memory write address channel
  memory.io.wChannel.wAddr.valid := l2Cache.io.mem.wChannel.wAddr.valid
  memory.io.wChannel.wAddr.bits := l2Cache.io.mem.wChannel.wAddr.bits(addressWidth - 1, log2Ceil(memBeatSize)) // Dummy memory accepts the address bits for bursts addresses only
  memory.io.wChannel.wStrb := l2Cache.io.mem.wChannel.wStrb
  l2Cache.io.mem.wChannel.wAddr.ready := memory.io.wChannel.wAddr.ready

  // Memory read and write data channels
  memory.io.rChannel.rData <> l2Cache.io.mem.rChannel.rData
  memory.io.rChannel.rLast <> l2Cache.io.mem.rChannel.rLast
  memory.io.wChannel.wData <> l2Cache.io.mem.wChannel.wData
  memory.io.wChannel.wLast <> l2Cache.io.mem.wChannel.wLast

  // Response demultiplexer
  val respDemux = Module(new CoreRespDemux(nCores, bytesPerSubBlock * 8, reqIdWidth))
  respDemux.io.sel := l2Cache.io.outCoreId
  respDemux.io.in := l2Cache.io.core.resp
  for (coreIdx <- 0 until nCores) {
    respDemux.io.resps(coreIdx) <> io.requests.cores(coreIdx).resp
  }
}