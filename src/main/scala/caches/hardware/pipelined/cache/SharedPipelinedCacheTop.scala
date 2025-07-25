package caches.hardware.pipelined.cache
import chisel3._
import caches.hardware.reppol._
import chisel3.util._

class SharedPipelinedCacheTop(
                         sizeInBytes: Int,
                         nWays: Int,
                         nCores: Int,
                         reqIdWidth: Int,
                         addressWidth: Int,
                         bytesPerBlock: Int,
                         bytesPerSubBlock: Int,
                         bytesPerBurst: Int,
                         l2RepPolicy: () => SharedCacheReplacementPolicyType
                       ) extends Module {
  require(isPow2(bytesPerBurst), "Bytes per burst need to be a power of 2.")

  val io = IO(new Bundle {
    val scheduler = new SchedulerIO(nCores)
    val cache = new CacheIO(nCores, reqIdWidth, addressWidth, bytesPerSubBlock * 8)
    val mem = new MemoryControllerIO(addressWidth, bytesPerBurst * 8)
  })

  // TODO: Add some sort of queue for rejected responses, to retry them at a later time
  //  or expect the core to re-attempt at a later time
  val l2Cache = Module(new SharedPipelinedCache(
    sizeInBytes = sizeInBytes,
    nWays = nWays,
    nCores = nCores,
    reqIdWidth = reqIdWidth,
    addressWidth = addressWidth,
    bytesPerBlock = bytesPerBlock,
    bytesPerSubBlock = bytesPerSubBlock,
    bytesPerBurst = bytesPerBurst
  ))

  val repPol = Module(l2RepPolicy())

  repPol.io.scheduler <> io.scheduler
  l2Cache.io.repPol <> repPol.io.control
  l2Cache.io.cache <> io.cache
  l2Cache.io.mem <> io.mem
}

object SharedPipelinedCacheTop extends App {
  val l2Size = 262144
  val l2Ways = 8
  val nCores = 4
  val reqIdWidth = 4
  val addressWidth = 32
  val l2BytesPerBlock = 64
  val l2BytesPerSubBlock = 16
  val l2BytesPerMemBurst = 4

  val l2nSets = l2Size / (l2Ways * l2BytesPerBlock)
  val l2RepPolicy = () => new BitPlruReplacementPolicy(l2Ways, l2nSets, nCores)

  (new chisel3.stage.ChiselStage).emitVerilog(
    new SharedPipelinedCacheTop(
      sizeInBytes = l2Size,
      nWays = l2Ways,
      nCores = nCores,
      reqIdWidth = reqIdWidth,
      addressWidth = addressWidth,
      bytesPerBlock = l2BytesPerBlock,
      bytesPerSubBlock = l2BytesPerSubBlock,
      bytesPerBurst = l2BytesPerMemBurst,
      l2RepPolicy = l2RepPolicy
    ),
    Array("--target-dir", "generated")
  )
}
