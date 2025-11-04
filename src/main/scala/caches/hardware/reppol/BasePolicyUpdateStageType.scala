package caches.hardware.reppol

import chisel3._
import chisel3.util._

abstract class BasePolicyUpdateStageType(nWays: Int, dataWidth: Int) extends Module {
  val io = IO(new Bundle {
    val hit = Input(Bool())
    val hitWay = Input(UInt(log2Up(nWays).W))
    val repWay = Input(UInt(log2Up(nWays).W))
    val stateIn = Input(UInt(dataWidth.W))
    val stateOut = Output(UInt(dataWidth.W))
    val repWayOut = Output(UInt(log2Up(nWays).W))
  })

  def update(repWay: UInt, state: UInt): UInt

  def getRepWay(state: UInt): UInt
}
