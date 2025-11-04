package caches.hardware.reppol

import caches.hardware.util.MemBlock
import chisel3._
import chisel3.util.{UIntToOH, log2Up}

class TreePlruReadStage(nWays: Int, nSets: Int, repSetFormat: BaseReplacementSetFormat) extends BasePolicyReadStageType(nWays, nSets, nWays - 1, repSetFormat) {
  val wayIdxBits = log2Up(nWays)

  val readLruBits = plruBits(rIdx = io.rIdx, wrEn = io.wrEn, wIdx = io.wIdx, wData = io.wData, stall = io.stall)

  val inState = WireDefault(0.U(nWays.W))
  when(io.fwd) {
    inState := io.wData
  }.otherwise {
    inState := readLruBits
  }

  val repSet = repSetFormat match {
    case NumericalFormat() => getNumericalReplacementSet(inState)
    case MruFormat() => getMruReplacementSet(inState)
    case _ => throw new IllegalArgumentException("Unrecognized replacement set format.")
  }

  def plruBits(rIdx: UInt, wrEn: Bool, wIdx: UInt, wData: UInt, stall: Bool): UInt = {
    val mruBits = Module(new MemBlock(nSets, nWays - 1))

    mruBits.io.readAddr := rIdx
    mruBits.io.writeAddr := wIdx
    mruBits.io.writeData := wData.asUInt
    mruBits.io.wrEn := wrEn
    mruBits.io.stall := stall

    mruBits.io.readData
  }

  override def getNumericalReplacementSet(state: UInt): Vec[UInt] = {
    val lruOrderedSet = VecInit(Seq.fill(nWays)(0.U(wayIdxBits.W)))

    lruOrderedSet(0) := getRepWay(state)

    lruOrderedSet
  }

  override def getMruReplacementSet(state: UInt): Vec[UInt] = {
    // TODO: Expand this to return actual replacement set
    val mruRepSet = VecInit(Seq.fill(nWays)(0.U(1.W)))
    val oneHot = UIntToOH(getRepWay(state))

    for (i <- 0 until mruRepSet.length) {
      mruRepSet(i) := oneHot(i).asUInt
    }

    mruRepSet
  }

  override def getRepWay(state: UInt): UInt = {
    val lru = VecInit(Seq.fill(wayIdxBits)(false.B))
    val treePath = VecInit(Seq.fill(wayIdxBits)(0.U(wayIdxBits.W)))

    for (i <- 0 until wayIdxBits) {
      val nodeState = state(treePath(i))
      lru(wayIdxBits - 1 - i) := nodeState // Set the MSB bits first
      if (i != wayIdxBits - 1) {
        val pathOffset = (treePath(i) << 1).asUInt

        when(nodeState === true.B) {
          treePath(i + 1) := pathOffset + 2.U
        }.otherwise {
          treePath(i + 1) := pathOffset + 1.U
        }
      }
    }

    lru.asUInt
  }

  io.readState := inState
  io.replaceWay := getRepWay(inState)
  io.replacementSet := repSet
}
