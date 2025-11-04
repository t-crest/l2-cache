package caches.hardware.reppol

import chisel3._
import chisel3.util.PriorityEncoder

class BitPlruUpdateStage(nWays: Int) extends BasePolicyUpdateStageType(nWays, nWays) {
  val newState = update(Mux(io.hit, io.hitWay, io.repWay), io.stateIn)

  override def update(hitWay: UInt, state: UInt): UInt = {
    val newMruBits = VecInit(Seq.fill(nWays)(false.B))

    // TODO: Need to find a way to reset the mru bits if the remaining 0 bits are owned by critical ways

    // Check for capacity
    val capacity = ((~state.asUInt).asUInt & ((~state.asUInt).asUInt - 1.U)) === 0.U

    for (bitIdx <- 0 until nWays) {
      when(capacity && (bitIdx.U =/= hitWay).asBool) { // When at capacity, reset all bits except the accessed way bit
        newMruBits(bitIdx) := false.B
      }.otherwise {
        newMruBits(bitIdx) := state(bitIdx)
      }
    }

    newMruBits(hitWay) := true.B // Update the accessed way

    newMruBits.asUInt
  }

  override def getRepWay(state: UInt): UInt = {
    // Find the first 0 bit by inverting the bit registers and passing them to the priority encoder
    PriorityEncoder((~state).asUInt)
  }

  io.stateOut := newState
  io.repWayOut := getRepWay(newState)
}
