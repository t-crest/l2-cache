package caches.hardware.reppol

trait BasePolicyType {
  def buildBasePolicyRead(nWays: Int, nSets: Int, repSetFormat: BaseReplacementSetFormat): BasePolicyReadStageType

  def buildBasePolicyUpdate(nWays: Int): BasePolicyUpdateStageType

  def getName: String
}

case class BitPlruType() extends BasePolicyType {
  override def buildBasePolicyRead(nWays: Int, nSets: Int, repSetFormat: BaseReplacementSetFormat): BasePolicyReadStageType = {
    new BitPlruReadStage(nWays, nSets, repSetFormat)
  }

  override def buildBasePolicyUpdate(nWays: Int): BasePolicyUpdateStageType = {
    new BitPlruUpdateStage(nWays)
  }

  override def getName: String = "BIT_PLRU"
}

case class TreePlruType() extends BasePolicyType {
  override def buildBasePolicyRead(nWays: Int, nSets: Int, repSetFormat: BaseReplacementSetFormat): BasePolicyReadStageType = {
    new TreePlruReadStage(nWays, nSets, repSetFormat)
  }

  override def buildBasePolicyUpdate(nWays: Int): BasePolicyUpdateStageType = {
    new TreePlruUpdateStage(nWays)
  }

  override def getName: String = "TREE_PLRU"
}

object BasePolicies {
  val BIT_PLRU = new BitPlruType

  val TREE_PLRU = new TreePlruType
}
