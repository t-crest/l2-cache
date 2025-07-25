package caches.hardware.reppol

import chisel3._
import chisel3.util._

/**
 * Tree based implementation of Pseudo LRU.
 *
 * @param nWays number of ways in a set-associate cache
 */
class TreePlruReplacementAlgorithm(nWays: Int) extends Module() {
  val io = IO(new Bundle {
    val update = Input(Valid(UInt(log2Up(nWays).W)))
    val replaceWay = Output(UInt(log2Up(nWays).W))
    val replacementSet = Output(Vec(nWays, UInt(log2Up(nWays).W)))
  })

  var wayIdxBits = log2Up(nWays)

  if (wayIdxBits % 2 != 0) {
    wayIdxBits += 1
  }

  // An array of one-bit registers, each for a node in the tree
  val plruBits = RegInit(VecInit(Seq.fill(nWays - 1)(false.B)))

  /**
   * Finds the index of the LRU way in the set
   *
   * @return the index of the LRU way in the set
   */
  def getLru: UInt = {
    val lru = VecInit(Seq.fill(wayIdxBits)(false.B))
    val treePath = VecInit(Seq.fill(wayIdxBits)(0.U(wayIdxBits.W)))

    for (i <- 0 until wayIdxBits) {
      val nodeState = plruBits(treePath(i))
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

  /**
   * Updates the LRU tree state based on the recently accessed way
   *
   * @param way the way that has been accessed
   */
  def updateLru(way: UInt): Unit = {
    val treePath = VecInit(Seq.fill(wayIdxBits)(0.U(wayIdxBits.W)))

    for (i <- 0 until wayIdxBits) {
      val accessBit = way(wayIdxBits - 1 - i)
      plruBits(treePath(i)) := ~accessBit

      if (i != wayIdxBits - 1) {
        val pathOffset = (treePath(i) << 1).asUInt

        when(accessBit === true.B) {
          treePath(i + 1) := pathOffset + 2.U
        }.otherwise {
          treePath(i + 1) := pathOffset + 1.U
        }
      }
    }
  }

  val replaceWay = getLru

  when(io.update.valid) {
    updateLru(io.update.bits)
  }

  val lruOrderedSet = VecInit(Seq.fill(nWays)(0.U(wayIdxBits.W)))
  lruOrderedSet(0) := replaceWay // The first element is the LRU way

  io.replaceWay := replaceWay
  io.replacementSet := lruOrderedSet
}
