package caches.hardware.reppol

import caches.hardware.reppol.ReplacementPolicyTest.performUpdateRequest
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TreePlruReplacementPolicyTest extends AnyFlatSpec with ChiselScalatestTester {
  "TreePlruReplacementPolicy" should "keep track of LRU way for 2 ways" in {
    val (nWays, nSets, nCores) = (2, 2, 1)
    test(new PolicyTestWrapper(() => new TreePlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val workingSet = 0
      dut.io.policy.control.setIdx.poke(workingSet.U)

      dut.clock.step()

      // Expect to point to the first way
      // Update LRU state by accessing first way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0, expectedRepCandidate = Some(0))

      // Expect to point to the second way
      // Update LRU state by accessing the second way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1, expectedRepCandidate = Some(1))

      // Expect to point to the first way
      // Update LRU state by accessing second way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1, expectedRepCandidate = Some(0))

      // Expect to point to the first way
      // Update LRU state by accessing first way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0, expectedRepCandidate = Some(0))

      // Expect to point to the second way
      // Update LRU state by accessing first way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0, expectedRepCandidate = Some(1))

      // Expect to point to the second way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1, expectedRepCandidate = Some(1))

      dut.clock.step()
    }
  }

  "TreePlruReplacementPolicy" should "keep track of LRU way for 4 ways" in {
    val (nWays, nSets, nCores) = (4, 2, 1)
    test(new PolicyTestWrapper(() => new TreePlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val workingSet = 1
      dut.io.policy.control.setIdx.poke(workingSet.U)

      dut.clock.step()

      // Expect to point to the first way
      // Update LRU on cache hit to way one
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0, expectedRepCandidate = Some(0))

      // Expect to point to the third way
      // Update LRU on cache hit to way three
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 2, expectedRepCandidate = Some(2))

      // Expect to point to the second way
      // Update LRU on cache hit to way two
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1, expectedRepCandidate = Some(1))

      // Expect to point to the fourth way
      // Update LRU on cache hit to way four
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 3, expectedRepCandidate = Some(3))

      // Expect to point to the first way
      // Update LRU on cache hit to way two
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1, expectedRepCandidate = Some(0))

      // Expect to point to the first third way
      // Update LRU on cache hit to way two
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1, expectedRepCandidate = Some(2))

      // Expect to point to the third way again
      // Update LRU on cache hit to way three
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 2, expectedRepCandidate = Some(2))

      // Expect to point to the first way now, since we had accessed way three recently
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 2, expectedRepCandidate = Some(0))

      dut.clock.step()
    }
  }

  "TreePlruReplacementPolicy" should "keep track of LRU way for 8 ways" in {
    val (nWays, nSets, nCores) = (8, 2, 1)
    test(new PolicyTestWrapper(() => new TreePlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.io.policy.control.setIdx.poke(0.U)

      dut.clock.step(3)

      dut.io.policy.control.replaceWay.expect(0.U)
      dut.io.policy.control.update.poke(true.B)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(4.U)
      dut.io.policy.control.update.poke(true.B)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(2.U)
      dut.io.policy.control.update.poke(true.B)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(6.U)
      dut.io.policy.control.update.poke(true.B)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(1.U)
      dut.io.policy.control.update.poke(true.B)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(5.U)
      dut.io.policy.control.update.poke(true.B)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(3.U)
      dut.io.policy.control.update.poke(true.B)
      dut.io.policy.control.setIdx.poke(1.U)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(7.U)
      dut.io.policy.control.update.poke(true.B)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(0.U)
      dut.io.policy.control.update.poke(false.B)
      dut.io.policy.control.setIdx.poke(0.U)

      //------------- set 1
      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(0.U)
      dut.io.policy.control.update.poke(true.B)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(4.U)
      dut.io.policy.control.update.poke(true.B)

      //------------- set 0
      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(0.U)
      dut.io.policy.control.update.poke(true.B)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(4.U)
      dut.io.policy.control.update.poke(true.B)

      dut.clock.step()
    }
  }
}
