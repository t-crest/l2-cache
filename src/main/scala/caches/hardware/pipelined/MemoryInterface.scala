package caches.hardware.pipelined

import caches.hardware.pipelined.stages.MemInterfaceToUpdateIO
import chisel3._
import chisel3.util._

/**
 * @param beatSize Beat size in bytes
 */
class ReadChannel(addrWidth: Int, beatSize: Int) extends Bundle {
  val rAddr = new DecoupledIO(UInt(addrWidth.W))
  val rData = Flipped(new DecoupledIO(UInt((beatSize * 8).W)))
  val rLast = Input(Bool())
}

/**
 * @param beatSize Beat size in bytes
 */
class WriteChannel(addrWidth: Int, beatSize: Int) extends Bundle {
  val wAddr = new DecoupledIO(UInt(addrWidth.W))
  val wData = new DecoupledIO(UInt((beatSize * 8).W))
  val wStrb = Output(UInt(beatSize.W))
  val wLast = Output(Bool())
}

/**
 * @param beatSize Beat size in bytes
 */
class CacheMemoryControllerIO(addrWidth: Int, beatSize: Int) extends Bundle {
  val rChannel = new ReadChannel(addrWidth, beatSize)
  val wChannel = new WriteChannel(addrWidth, beatSize)
}

/**
 * Logic block responsible for interfacing between the cache and the higher level memory block.
 *
 * @param nCores           Number of cores connected to the cache
 * @param nWays            Number of ways in the cache
 * @param reqIdWidth       Request ID width
 * @param tagWidth         Tag width
 * @param indexWidth       Index width
 * @param blockOffsetWidth Block offset width
 * @param blockWidth       Width of a single cache line
 * @param subBlockWidth    Width of a cache line sub-block
 * @param beatSize         Size of a single beat in bytes
 * @param burstLen         Number of beats in a single transfer
 */
class MemoryInterface(nCores: Int, nWays: Int, nHalfMissCmds: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, blockWidth: Int, subBlockWidth: Int, beatSize: Int, burstLen: Int) extends Module {
  require((blockWidth / 8) >= (beatSize * burstLen), "Block size must be greater or equal to the total size of a single memory command.")

  private val byteOffsetWidth = log2Up(subBlockWidth / 8)
  private val addressWidth = tagWidth + indexWidth + blockOffsetWidth + byteOffsetWidth
  private val nSubBlocks = blockWidth / subBlockWidth
  private val bytesPerCmd = beatSize * burstLen
  private val nCommands = (blockWidth / 8) / bytesPerCmd

  val io = IO(new Bundle {
    val missFifo = Flipped(new MshrPopIO(nCores, nHalfMissCmds, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, blockWidth))
    val missCritEmpty = Input(Bool())
    val missNonCritEmpty = Input(Bool())
    val wbFifo = Flipped(new WbFifoPopIO(tagWidth, indexWidth, blockWidth))
    val wbCritEmpty = Input(Bool())
    val wbNonCritEmpty = Input(Bool())
    val updateLogic = Flipped(new MemInterfaceToUpdateIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockWidth, subBlockWidth))
    val memController = new CacheMemoryControllerIO(addressWidth, beatSize)
    val popQSel = Output(Bool())
  })

  val sIdle :: sRead :: sWrite :: sReadBurst :: sWriteBurst :: sDoneRead :: sDoneWrite :: Nil = Enum(7)
  val stateReg = RegInit(sIdle)

  // Registers
  val memRDataReg = RegInit(VecInit(Seq.fill(blockWidth / (beatSize * 8))(0.U((beatSize * 8).W))))
  val totalBurstCount = RegInit(0.U(log2Up(blockWidth / (beatSize * 8)).W))
  val cmdBurstCount = RegInit(0.U(log2Up(beatSize).W))
  val cmdCounter = RegInit(0.U((log2Up(nCommands) + 1).W)) // RAM command counter
  val halfMissCmdCounter = RegInit(0.U((log2Up(nHalfMissCmds) + 1).W)) // Half of the miss command counter
  val popQSelReg = RegInit(0.U(1.W)) // 0 - non-crit, 1 - crit

  val reqIdx = WireDefault(0.U(indexWidth.W))
  val reqTag = WireDefault(0.U(tagWidth.W))

  // Default signal assignments
  val memRAddrValid = WireDefault(false.B)
  val memRDataReady = WireDefault(false.B)
  val memWAddrValid = WireDefault(false.B)
  val memWDataValid = WireDefault(false.B)
  val memWData = WireDefault(0.U((beatSize * 8).W))
  val memWLast = WireDefault(false.B)
  val memWStrb = WireDefault(0.U(beatSize.W))
  val memRDataRegAsUint = memRDataReg.asUInt
  val wbFifoWDataAsVec = VecInit(Seq.fill(blockWidth / (beatSize * 8))(0.U((beatSize * 8).W)))

  for (i <- 0 until wbFifoWDataAsVec.length) {
    wbFifoWDataAsVec(i) := io.wbFifo.popEntry.wbData(((beatSize * 8) - 1) + (i * (beatSize * 8)), i * (beatSize * 8))
  }

  val wbFifoPop = WireDefault(false.B)
  val missFifoPop = WireDefault(false.B)
  val updateLogicValid = WireDefault(false.B)
  val updateLogicValidCmd = WireDefault(false.B)

  switch(stateReg) {
    is(sIdle) {
      when(!io.wbCritEmpty) {
        stateReg := sWrite
        popQSelReg := 1.U
      }.elsewhen(!io.missCritEmpty) {
        stateReg := sRead
        popQSelReg := 1.U
      }.elsewhen(!io.wbNonCritEmpty) {
        stateReg := sWrite
        popQSelReg := 0.U
      }.elsewhen(!io.missNonCritEmpty) {
        stateReg := sRead
        popQSelReg := 0.U
      }
    }

    is(sRead) {
      when(cmdCounter === nCommands.U) {
        cmdCounter := 0.U
        stateReg := sDoneRead
      }.otherwise {
        memRAddrValid := true.B
        reqTag := io.missFifo.popEntry.tag
        reqIdx := io.missFifo.popEntry.index

        when(io.memController.rChannel.rAddr.ready) {
          stateReg := sReadBurst
        }
      }
    }

    is(sWrite) {
      when(cmdCounter === nCommands.U) {
        cmdCounter := 0.U
        stateReg := sDoneWrite
      }.otherwise {
        memWAddrValid := true.B
        reqTag := io.wbFifo.popEntry.tag
        reqIdx := io.wbFifo.popEntry.index

        when(io.memController.wChannel.wAddr.ready) {
          stateReg := sWriteBurst
        }
      }
    }

    is(sReadBurst) {
      memRDataReady := true.B
      when(io.memController.rChannel.rData.valid) {
        totalBurstCount := totalBurstCount + 1.U
        cmdBurstCount := cmdBurstCount + 1.U

        memRDataReg(totalBurstCount) := io.memController.rChannel.rData.bits

        when(io.memController.rChannel.rLast) {
          cmdCounter := cmdCounter + 1.U
          stateReg := sRead
        }
      }
    }

    is(sWriteBurst) {
      memWDataValid := true.B
      memWData := wbFifoWDataAsVec(totalBurstCount)
      memWStrb := (math.pow(2, subBlockWidth / 8).toInt - 1).U // Set all bytes in the sub-block to be written
      // NOTE: set this to all ones or zeros based on whether the sub-block is dirty or not

      when(io.memController.wChannel.wData.ready) {
        totalBurstCount := totalBurstCount + 1.U
        cmdBurstCount := cmdBurstCount + 1.U

        when(cmdBurstCount === (burstLen - 1).U) {
          memWLast := true.B
          cmdCounter := cmdCounter + 1.U
          stateReg := sWrite
        }
      }
    }

    is(sDoneRead) {
      updateLogicValid := true.B

      when(halfMissCmdCounter === io.missFifo.cmdCnt) {
        halfMissCmdCounter := 0.U
        missFifoPop := true.B
        stateReg := sIdle
        totalBurstCount := 0.U
      }.otherwise {
        updateLogicValidCmd := true.B
        halfMissCmdCounter := halfMissCmdCounter + 1.U
      }
    }

    is(sDoneWrite) {
      wbFifoPop := true.B
      stateReg := sIdle
      totalBurstCount := 0.U
    }
  }

  io.wbFifo.pop := wbFifoPop
  io.missFifo.pop := missFifoPop

  io.updateLogic.valid := updateLogicValid
  io.updateLogic.validCmd := updateLogicValidCmd
  io.updateLogic.tag := io.missFifo.popEntry.tag
  io.updateLogic.index := io.missFifo.popEntry.index
  io.updateLogic.wWay := io.missFifo.popEntry.replaceWay
  io.updateLogic.byteEn := io.missFifo.popEntry.byteEn
  io.updateLogic.blockOffset := io.missFifo.cmds(halfMissCmdCounter).blockOffset
  io.updateLogic.reqId := io.missFifo.cmds(halfMissCmdCounter).reqId
  io.updateLogic.coreId := io.missFifo.cmds(halfMissCmdCounter).coreId

  for (i <- 0 until nSubBlocks) {
    io.updateLogic.memReadData(i) := memRDataRegAsUint((subBlockWidth - 1) + (i * subBlockWidth), i * subBlockWidth)
  }

  val outAddr = if (nCommands > 1) {
    Cat(reqTag, reqIdx, cmdCounter(log2Up(nCommands) - 1, 0)) << log2Up(bytesPerCmd).U
  } else {
    Cat(reqTag, reqIdx) << log2Up(bytesPerCmd).U
  }

  io.memController.rChannel.rAddr.valid := memRAddrValid
  io.memController.rChannel.rAddr.bits := outAddr
  io.memController.rChannel.rData.ready := memRDataReady
  io.memController.wChannel.wAddr.valid := memWAddrValid
  io.memController.wChannel.wAddr.bits := outAddr
  io.memController.wChannel.wData.valid := memWDataValid
  io.memController.wChannel.wData.bits := memWData
  io.memController.wChannel.wStrb := memWStrb
  io.memController.wChannel.wLast := memWLast

  io.popQSel := popQSelReg
}
