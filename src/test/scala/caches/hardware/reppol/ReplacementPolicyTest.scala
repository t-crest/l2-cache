package caches.hardware.reppol

import chisel3._
import chiseltest._

object ReplacementPolicyTest {
  def assertNumericalReplacementSet(dut: PolicyTestWrapper, expectedSet: Array[Int], printActual: Boolean = false): Unit = {
    if (printActual) println("")

    val idxs = dut.io.dbg.repSet
    for (i <- 0 until idxs.length) {
      if (printActual) println(s"Way $i: ${idxs(i).peekInt()}")

      val actualWay = idxs(i).peekInt()
      val expectedWay = expectedSet(i)
      assert(actualWay == expectedWay, s"Actual way: $actualWay for index $i, did not equal the expected way: $expectedWay")
    }

    if (printActual) println("")
  }

  def setCoreAsCritical(dut: PolicyTestWrapper, coreID: Int, wData: Int): Unit = {
    dut.io.policy.scheduler.cmd.poke(SchedulerCmd.WR)
    dut.io.policy.scheduler.addr.poke(coreID.U)
    dut.io.policy.scheduler.wData.poke(wData.U)

    dut.clock.step()

    // Reset the signals
    dut.io.policy.scheduler.cmd.poke(SchedulerCmd.NULL)
    dut.io.policy.scheduler.addr.poke(0.U)
    dut.io.policy.scheduler.wData.poke(0.U)
  }

  def unsetCoreAsCritical(dut: PolicyTestWrapper, coreID: Int): Unit = {
    dut.io.policy.scheduler.cmd.poke(SchedulerCmd.RD)
    dut.io.policy.scheduler.addr.poke(coreID.U)

    dut.clock.step()

    // Reset the signals
    dut.io.policy.scheduler.cmd.poke(SchedulerCmd.NULL)
    dut.io.policy.scheduler.addr.poke(0.U)
  }

  def performEvictionRequest(dut: PolicyTestWrapper, coreId: Int, setIdx: Int, expectedEvictionCandidate: Option[Int] = None): Unit = {
    // Make a request on behalf of the requesting core index
    dut.io.policy.control.setIdx.poke(setIdx.U)

    dut.clock.step() // Need a two clock cycle delay here since the base policy is separated from contention logic by two pipeline stage

    dut.io.policy.control.setIdx.poke(0.U)
    dut.io.policy.control.coreId.poke(coreId.U)

    dut.clock.step()

    dut.io.policy.control.coreId.poke(0.U)

    dut.clock.step()

    dut.io.policy.control.evict.poke(true.B)

    dut.io.policy.control.isValid.expect(expectedEvictionCandidate.map(_ => true.B).getOrElse(false.B))
    if (expectedEvictionCandidate.isDefined) {
      dut.io.policy.control.replaceWay.expect(expectedEvictionCandidate.map((x: Int) => x.U).getOrElse(0.U))
    }

    dut.clock.step()

    dut.io.policy.control.evict.poke(false.B)
    dut.io.policy.control.setIdx.poke(0.U)
  }

  def performUpdateRequest(dut: PolicyTestWrapper, coreId: Int, setIdx: Int, hitWay: Int, expectedRepCandidate: Option[Int] = None, expectedSet: Option[Array[Int]] = None): Unit = {
    // Make a request on behalf of the requesting core index
    dut.io.policy.control.setIdx.poke(setIdx.U)

    dut.clock.step() // Need a two clock cycle delay here since the base policy is separated from contention logic by two pipeline stage

    dut.io.policy.control.setIdx.poke(0.U)
    dut.io.policy.control.coreId.poke(coreId.U)

    dut.clock.step()

    dut.io.policy.control.coreId.poke(0.U)
    dut.io.policy.info.isHit.poke(true.B)
    dut.io.policy.info.hitWay.poke(hitWay.U)

    dut.clock.step()

    dut.io.policy.info.isHit.poke(false.B)
    dut.io.policy.info.hitWay.poke(0.U)
    dut.io.policy.control.update.poke(true.B)

    if (expectedRepCandidate.isDefined) {
      dut.io.policy.control.replaceWay.expect(expectedRepCandidate.map((x: Int) => x.U).getOrElse(0.U))
    }

    if (expectedSet.isDefined) {
      assertNumericalReplacementSet(dut, expectedSet.get)
    }

    dut.clock.step()

    dut.io.policy.control.update.poke(false.B)
  }
}
