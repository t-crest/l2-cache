package caches.hardware.reppol

import caches.hardware.reppol.ReplacementPolicyTest._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable

object LeastCriticalReplacementPolicyTest {
  /** A request to the policy. A miss allocates a line (evict and update), a hit only updates. */
  case class Req(coreId: Int, setIdx: Int, hitWay: Option[Int] = None)

  def miss(coreId: Int, setIdx: Int = 0): Req = Req(coreId, setIdx)

  def hit(coreId: Int, hitWay: Int, setIdx: Int = 0): Req = Req(coreId, setIdx, Some(hitWay))

  /** Returned instead of a way for a rejected request. */
  val Rejected = -1

  /**
   * Issues the requests back-to-back, one per cycle, as the cache pipeline does: set index in cycle 0, core ID in
   * cycle 1, hit information in cycle 2, and update/evict in cycle 3, where the replacement way is read. Like the
   * cache, a miss that the policy rejects (isValid low) neither updates the policy nor evicts a line.
   *
   * @param allowReject if false, every request must be accepted
   * @return the replacement way given to each request, or Rejected
   */
  def runPipelined(dut: PolicyTestWrapper, reqs: Seq[Req], allowReject: Boolean = false): Seq[Int] = {
    val repWays = mutable.ArrayBuffer.fill(reqs.length)(-1)

    for (cycle <- 0 until reqs.length + 3) {
      val ctrl = dut.io.policy.control
      val info = dut.io.policy.info

      ctrl.setIdx.poke(reqs.lift(cycle).map(_.setIdx).getOrElse(0).U)
      ctrl.valid.poke(reqs.lift(cycle - 1).isDefined.B)
      ctrl.coreId.poke(reqs.lift(cycle - 1).map(_.coreId).getOrElse(0).U)
      info.isHit.poke(reqs.lift(cycle - 2).exists(_.hitWay.isDefined).B)
      info.hitWay.poke(reqs.lift(cycle - 2).flatMap(_.hitWay).getOrElse(0).U)

      val wbReq = reqs.lift(cycle - 3)
      val isHit = wbReq.exists(_.hitWay.isDefined)
      val accepted = wbReq.isDefined && (isHit || ctrl.isValid.peekBoolean())
      ctrl.update.poke(accepted.B)
      ctrl.evict.poke((accepted && !isHit).B)
      if (wbReq.isDefined) {
        assert(accepted || allowReject, s"Request ${cycle - 3} was rejected")
        repWays(cycle - 3) = if (accepted) ctrl.replaceWay.peekInt().toInt else Rejected
      }

      dut.clock.step()
    }

    val ctrl = dut.io.policy.control
    ctrl.update.poke(false.B)
    ctrl.evict.poke(false.B)
    ctrl.valid.poke(false.B)
    dut.clock.step()

    repWays.toSeq
  }

  /** Issues the requests one at a time, with no other request in the pipeline. */
  def runSequential(dut: PolicyTestWrapper, reqs: Seq[Req], allowReject: Boolean = false): Seq[Int] =
    reqs.flatMap(req => runPipelined(dut, Seq(req), allowReject))
}

class LeastCriticalReplacementPolicyTest extends AnyFlatSpec with ChiselScalatestTester {

  import LeastCriticalReplacementPolicyTest._

  val (nWays, nSets, nCores) = (4, 2, 4)

  def lcPolicy(claimOnHit: Boolean = true, rejectNonCritical: Boolean = false) =
    new PolicyTestWrapper(() => new LeastCriticalReplacementPolicy(nWays, nSets, nCores, BasePolicies.BIT_PLRU, claimOnHit, rejectNonCritical))

  "LeastCriticalReplacementPolicy" should "behave like its base policy when no core is critical" in {
    test(lcPolicy()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val workingSet = 1
      dut.io.policy.control.setIdx.poke(workingSet.U)
      dut.clock.step()

      // Same sequence as the bit PLRU test
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0, expectedRepCandidate = Some(0), expectedSet = Some(Array(0, 1, 2, 3)))
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 2, expectedRepCandidate = Some(1), expectedSet = Some(Array(1, 2, 3, 0)))
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1, expectedRepCandidate = Some(1), expectedSet = Some(Array(1, 3, 0, 2)))
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 3, expectedRepCandidate = Some(3), expectedSet = Some(Array(3, 0, 1, 2)))
    }
  }

  "LeastCriticalReplacementPolicy" should "not evict critical lines while non-critical lines exist" in {
    test(lcPolicy()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      setCoreAsCritical(dut, coreID = 0, wData = 1)

      // Core 0 allocates ways 0 and 1
      assert(runSequential(dut, Seq(miss(0), miss(0))) == Seq(0, 1))

      // Core 1 only gets ways 2 and 3. For the third and fourth miss, plain bit PLRU would pick ways 0 and 1. Bit PLRU
      // does not order the recently used ways, so way 2 is picked twice
      assert(runSequential(dut, Seq(miss(1), miss(1), miss(1), miss(1))) == Seq(2, 3, 2, 2))

      // Other sets are not affected
      assert(runSequential(dut, Seq(miss(1, setIdx = 1), miss(1, setIdx = 1))) == Seq(0, 1))
    }
  }

  "LeastCriticalReplacementPolicy" should "evict the first line when all lines are critical, also for non-critical cores" in {
    test(lcPolicy()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      setCoreAsCritical(dut, coreID = 0, wData = 1)

      // Core 0 fills the set; the bit PLRU state is reset to only way 3 being recently used
      assert(runSequential(dut, Seq(miss(0), miss(0), miss(0), miss(0))) == Seq(0, 1, 2, 3))

      // A non-critical core is not rejected and evicts the first line in base policy order
      assert(runSequential(dut, Seq(miss(1))) == Seq(0))

      // Way 0 is now non-critical, so it is the only candidate for core 2
      assert(runSequential(dut, Seq(miss(2), miss(2))) == Seq(0, 0))
    }
  }

  "LeastCriticalReplacementPolicy" should "release the lines of a core that is no longer critical" in {
    test(lcPolicy()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      setCoreAsCritical(dut, coreID = 0, wData = 1)
      assert(runSequential(dut, Seq(miss(0), miss(0), miss(1), miss(1))) == Seq(0, 1, 2, 3))
      assert(runSequential(dut, Seq(miss(1))) == Seq(2))

      // Read returns whether the core was critical, and unsets it
      dut.io.policy.scheduler.cmd.poke(SchedulerCmd.RD)
      dut.io.policy.scheduler.addr.poke(0.U)
      dut.io.policy.scheduler.rData.expect(1.U)
      dut.clock.step()
      dut.io.policy.scheduler.cmd.poke(SchedulerCmd.NULL)

      // Now plain bit PLRU order: way 0 is the least recently used
      assert(runSequential(dut, Seq(miss(1))) == Seq(0))
    }
  }

  "LeastCriticalReplacementPolicy" should "protect a non-critical line once a critical core hits it" in {
    test(lcPolicy()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      setCoreAsCritical(dut, coreID = 0, wData = 1)

      // Core 1 fills the set, then core 0 hits way 2
      assert(runSequential(dut, Seq(miss(1), miss(1), miss(1), miss(1))) == Seq(0, 1, 2, 3))
      runSequential(dut, Seq(hit(0, hitWay = 2)))

      // Way 2 is skipped from now on
      assert(!runSequential(dut, Seq.fill(6)(miss(1))).contains(2))
    }
  }

  "LeastCriticalReplacementPolicy" should "not protect hit lines when claimOnHit is disabled" in {
    test(lcPolicy(claimOnHit = false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      setCoreAsCritical(dut, coreID = 0, wData = 1)

      assert(runSequential(dut, Seq(miss(1), miss(1), miss(1), miss(1))) == Seq(0, 1, 2, 3))
      runSequential(dut, Seq(hit(0, hitWay = 2)))

      assert(runSequential(dut, Seq.fill(6)(miss(1))).contains(2))
    }
  }

  "LeastCriticalReplacementPolicy" should "forward line assignments to back-to-back requests" in {
    test(lcPolicy()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      setCoreAsCritical(dut, coreID = 0, wData = 1)

      // Core 0 owns ways 0 and 1, core 1 owns ways 2 and 3; base order is 0, 1, 2, 3
      assert(runSequential(dut, Seq(miss(0), miss(0), miss(1), miss(1))) == Seq(0, 1, 2, 3))

      // Core 0 takes way 2. The following misses of core 1 are one, two and three cycles behind it, so they see the
      // new assignment of way 2 through the update stage forwarding, the memory forwarding, and the memory
      runPipelined(dut, Seq(miss(0), miss(1), miss(1), miss(1))) match {
        case Seq(first, rest@_*) =>
          assert(first == 2)
          assert(rest.forall(_ == 3), s"Core 1 evicted a critical line: $rest")
      }
    }
  }

  "LeastCriticalReplacementPolicy with rejection" should "reject non-critical misses to a fully critical set" in {
    test(lcPolicy(rejectNonCritical = true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      setCoreAsCritical(dut, coreID = 0, wData = 1)
      assert(runSequential(dut, Seq.fill(4)(miss(0))) == Seq(0, 1, 2, 3))

      // Core 1 is rejected as long as all ways are critical
      assert(runSequential(dut, Seq(miss(1), miss(1)), allowReject = true) == Seq(Rejected, Rejected))

      // The critical core is never rejected, and the rejections did not change the base policy state
      assert(runSequential(dut, Seq(miss(0))) == Seq(0))

      // Once core 0 is no longer critical, core 1 is accepted
      unsetCoreAsCritical(dut, coreID = 0)
      assert(runSequential(dut, Seq(miss(1))) == Seq(1))
    }
  }

  "LeastCriticalReplacementPolicy with rejection" should "accept non-critical misses while a non-critical way exists" in {
    test(lcPolicy(rejectNonCritical = true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      setCoreAsCritical(dut, coreID = 0, wData = 1)
      assert(runSequential(dut, Seq(miss(0), miss(0), miss(0))) == Seq(0, 1, 2))
      assert(runSequential(dut, Seq.fill(4)(miss(1))) == Seq(3, 3, 3, 3))
    }
  }

  "LeastCriticalReplacementPolicy with rejection" should "see the line assignment of the request ahead of it" in {
    test(lcPolicy(rejectNonCritical = true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      setCoreAsCritical(dut, coreID = 0, wData = 1)
      assert(runSequential(dut, Seq(miss(0), miss(0), miss(0), miss(1))) == Seq(0, 1, 2, 3))

      // Core 0 takes the last non-critical way, so core 1 directly behind it must be rejected
      assert(runPipelined(dut, Seq(miss(0), miss(1)), allowReject = true) == Seq(3, Rejected))
    }
  }
}
