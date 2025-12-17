package caches.hardware.pipelined

import chisel3._
import chisel3.util._
import ocp._

class OcpCacheWrapperPort(
                           nCores: Int,
                           addrWidth: Int,
                           coreDataWidth: Int,
                           coreBurstLen: Int,
                           memDataWidth: Int,
                           memBurstLen: Int,
                           schedulerDataWidth: Int
                         ) extends Bundle {
  val cores = Vec(nCores, new OcpBurstSlavePort(addrWidth, coreDataWidth, coreBurstLen))
  val mem = new OcpBurstMasterPort(addrWidth, memDataWidth, memBurstLen)
  val scheduler = new OcpCoreSlavePort(log2Up(nCores), schedulerDataWidth)
}

/**
 * Wrapper that wraps the l2 cache into the OCP interface.
 */
class OcpCacheWrapper(
                       nCores: Int,
                       addrWidth: Int,
                       coreDataWidth: Int,
                       coreBurstLen: Int,
                       memDataWidth: Int,
                       memBurstLen: Int,
                       l2Cache: () => SharedPipelinedCache
                     ) extends Module {

  val cache = Module(l2Cache())
  val l2SchedulerDataWidth = cache.schedulerDataWidth

  val io = IO(new OcpCacheWrapperPort(nCores, addrWidth, coreDataWidth, coreBurstLen, memDataWidth, memBurstLen, l2SchedulerDataWidth))

  val coresOcpAdapter = Array.fill(nCores)(Module(new OcpBurstSlaveToCacheRequestAdapter(addrWidth, coreDataWidth, coreBurstLen)))
  val memOcpAdapter = Module(new CacheMemToOcpBurstMasterAdapter(addrWidth, memDataWidth, memBurstLen))
  val schedulerOcpAdapter = Module(new OcpCoreSlaveToSchedulerAdapter(nCores, l2SchedulerDataWidth))

  // Connection between the scheduler and the cache through the OCP interface
  schedulerOcpAdapter.io.core <> io.scheduler
  cache.io.scheduler <> schedulerOcpAdapter.io.scheduler

  if (nCores > 1) {
    val rrArbiter = Module(new RoundRobinRequestArbiter(nCores, addrWidth, coreDataWidth * coreBurstLen, 1))

    for (coreIdx <- 0 until nCores) {
      // Connect each cores OCP interface to the OCP adapter
      coresOcpAdapter(coreIdx).io.ocpBurst <> io.cores(coreIdx)

      // Connect each cores OCP adapter to the request arbiter
      rrArbiter.io.ports(coreIdx).reqId <> coresOcpAdapter(coreIdx).io.corePort.req.reqId
      rrArbiter.io.ports(coreIdx).addr := coresOcpAdapter(coreIdx).io.corePort.req.addr
      rrArbiter.io.ports(coreIdx).rw := coresOcpAdapter(coreIdx).io.corePort.req.rw
      rrArbiter.io.ports(coreIdx).byteEn := coresOcpAdapter(coreIdx).io.corePort.req.byteEn
      rrArbiter.io.ports(coreIdx).wData := coresOcpAdapter(coreIdx).io.corePort.req.wData
    }

    // Connection between request arbiter and the cache
    cache.io.core.req.reqId <> rrArbiter.io.out.reqId
    cache.io.core.req.rw <> rrArbiter.io.out.rw
    cache.io.core.req.addr <> rrArbiter.io.out.addr
    cache.io.core.req.wData <> rrArbiter.io.out.wData
    cache.io.core.req.byteEn <> rrArbiter.io.out.byteEn
    cache.io.inCoreId := rrArbiter.io.chosen

    // Core response demultiplexer
    val respDemux = Module(new CoreRespDemux(nCores, cache.l2CacheBytesPerSubBlock * 8, 1))
    respDemux.io.sel := RegNext(cache.io.outCoreId) // Reduce the critical path
    respDemux.io.in := RegNext(cache.io.core.resp) // Reduce the critical path

    for (coreIdx <- 0 until nCores) {
      // Connect the demultiplexer to each core OCP adapter response interface
      coresOcpAdapter(coreIdx).io.corePort.resp <> respDemux.io.resps(coreIdx)
    }

    // Connection between the cache and the memory through the OCP interface
    cache.io.mem <> memOcpAdapter.io.cache
    io.mem <> memOcpAdapter.io.ocpBurst
  } else {
    cache.io.inCoreId := 0.U
    coresOcpAdapter(0).io.ocpBurst <> io.cores
    cache.io.core <> coresOcpAdapter(0).io.corePort

    cache.io.mem <> memOcpAdapter.io.cache
    io.mem <> memOcpAdapter.io.ocpBurst
  }
}
