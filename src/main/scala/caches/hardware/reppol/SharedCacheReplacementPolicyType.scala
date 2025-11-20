package caches.hardware.reppol

import chisel3._
import chisel3.util._

trait BaseReplacementSetFormat {
  def getName: String
}

case class MruFormat() extends BaseReplacementSetFormat {
  override def getName: String = "MRU_FORMAT"
}

case class NumericalFormat() extends BaseReplacementSetFormat {
  override def getName: String = "NUMERICAL_FORMAT"
}

object SchedulerCmd {
  val schedulerCmdWidth = 2

  val NULL = "b00".U(schedulerCmdWidth.W)
  val RD = "b01".U(schedulerCmdWidth.W)
  val WR = "b10".U(schedulerCmdWidth.W)
}

class SchedulerControlIO(nCores: Int, dataWidth: Int) extends Bundle {
  val cmd = Input(UInt(2.W))
  val addr = Input(UInt(log2Up(nCores).W))
  val wData = Input(UInt(dataWidth.W))
  val rData = Output(UInt(dataWidth.W))
}

class ReplacementPolicyInfoIO(nWays: Int, missQueueDepth: Int) extends Bundle {
  val isHit = Input(Bool())
  val hitWay = Input(UInt(log2Up(nWays).W))
  val nonCritMisses = Input(UInt(log2Up(missQueueDepth).W))
  val nonCritWbs = Input(UInt(log2Up(missQueueDepth).W))
  val isReplacementWayCrit = Output(Bool())
  val isReplacementWayAtLimit = Output(Bool())
  val updateCoreReachedLimit = Output(Bool())
  val updateCoreIsCrit = Output(Bool())
}

class ReplacementPolicyControlIO(nCores: Int, nWays: Int, nSets: Int) extends Bundle {
  val valid = Input(Bool())
  val stall = Input(Bool())
  val evict = Input(Bool()) // Some policies may need to know if when the line is being evicted
  val update = Input(Bool())
  val coreId = Input(UInt(log2Up(nCores).W))
  val setIdx = Input(UInt(log2Up(nSets).W))
  val isValid = Output(Bool()) // To signal if there are no valid ways to replace
  val replaceWay = Output(UInt(log2Up(nWays).W))
  val insertBubble = Output(Bool()) // To signal that a bubble must be inserted
}

class SharedCacheReplacementIO(nWays: Int, nSets: Int, nCores: Int, schedulerDataWidth: Int, missQueueDepth: Int = 4) extends Bundle {
  val control = new ReplacementPolicyControlIO(nCores, nWays, nSets)
  val info = new ReplacementPolicyInfoIO(nWays, missQueueDepth)
  val scheduler = new SchedulerControlIO(nCores, schedulerDataWidth)
}

/**
 * A replacement policy type for a shared set associate cache.
 *
 * @param nWays          number of ways in a single cache set
 * @param nSets          number of sets in the whole cache
 * @param nCores         number of cores sharing the cache
 * @param dataWidth      scheduler data transfer width
 * @param missQueueDepth number of queue elements in the miss fifo
 * @param repSetFormat   replacement set representation format
 */
abstract class SharedCacheReplacementPolicyType(
                                                 nWays: Int,
                                                 nSets: Int,
                                                 nCores: Int,
                                                 dataWidth: Int = 1,
                                                 missQueueDepth: Int = 4,
                                                 repSetFormat: BaseReplacementSetFormat = new NumericalFormat
                                               ) extends Module {
  val io = IO(new SharedCacheReplacementIO(nWays, nSets, nCores, dataWidth, missQueueDepth))

  val repSet = getDefaultRepSet

  def getWays: Int = {
    nWays
  }

  def getSets: Int = {
    nSets
  }

  def getCores: Int = {
    nCores
  }

  def getSchedulerDataWidth: Int = {
    dataWidth
  }

  def getReplacementSetFormat: BaseReplacementSetFormat = {
    repSetFormat
  }

  def getDefaultRepSet: Vec[UInt] = {
    repSetFormat match {
      case NumericalFormat() => VecInit(Seq.fill(nWays)(0.U(log2Up(nWays).W)))
      case MruFormat() => VecInit(Seq.fill(nWays)(0.U(1.W)))
    }
  }

  def includeCriticalMissQ(): Boolean

  def includeCriticalWbQ(): Boolean

  def printConfig(): Unit
}
