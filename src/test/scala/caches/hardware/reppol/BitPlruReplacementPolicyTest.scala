package caches.hardware.reppol

import caches.hardware.reppol.ReplacementPolicyTest._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class BitPlruReplacementPolicyTest extends AnyFlatSpec with ChiselScalatestTester {
  "BitPlruReplacementPolicy" should "keep track LRU way for 2 ways" in {
    val (nWays, nSets, nCores) = (2, 2, 1)
    test(new PolicyTestWrapper(() => new BitPlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val workingSet = 0
      dut.io.policy.control.setIdx.poke(workingSet.U)

      dut.clock.step()

      // Update LRU state by accessing first way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0, expectedRepCandidate = Some(0))

      // Update LRU state by accessing the second way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1, expectedRepCandidate = Some(1))

      // Update LRU state by accessing second way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1, expectedRepCandidate = Some(0))

      // Update LRU state by accessing first way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0, expectedRepCandidate = Some(0))

      // Update LRU state by accessing first way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0, expectedRepCandidate = Some(1))

      dut.clock.step()
    }
  }

  "BitPlruReplacementPolicy" should "keep track of LRU way for 4 ways" in {
    val (nWays, nSets, nCores) = (4, 2, 1)
    test(new PolicyTestWrapper(() => new BitPlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val workingSet = 1
      dut.io.policy.control.setIdx.poke(workingSet.U)

      dut.clock.step()

      // Expect to point to the first way
      // Update LRU state by accessing first way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0, expectedRepCandidate = Some(0), expectedSet = Some(Array(0, 1, 2, 3)))

      // Expect to point to the second way
      // Update LRU state by accessing third way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 2, expectedRepCandidate = Some(1), expectedSet = Some(Array(1, 2, 3, 0)))

      // Expect to point to the second way
      // Update LRU state by accessing second way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1, expectedRepCandidate = Some(1), expectedSet = Some(Array(1, 3, 0, 2)))

      // Expect to point to the fourth way
      // Update LRU state by accessing fourth way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 3, expectedRepCandidate = Some(3), expectedSet = Some(Array(3, 0, 1, 2)))

      // Expect to point to the first way
      // Update LRU state by accessing fourth way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 3, expectedRepCandidate = Some(0), expectedSet = Some(Array(0, 1, 2, 3)))

      dut.clock.step()
    }
  }

  "BitPlruReplacementPolicy" should "keep track of LRU way for 8 ways" in {
    val (nWays, nSets, nCores) = (8, 2, 1)
    test(new PolicyTestWrapper(() => new BitPlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.policy.control.setIdx.poke(0.U)

      dut.clock.step(3)

      dut.io.policy.control.replaceWay.expect(0.U)
      //      assertNumericalReplacementSet(dut, expectedSet = Array(0, 1, 2, 3, 4, 5, 6, 7), printActual = true)
      dut.io.policy.control.update.poke(true.B)
      dut.io.policy.info.hitWay.poke(2.U)
      dut.io.policy.info.isHit.poke(true.B)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(1.U)
      //      assertNumericalReplacementSet(dut, expectedSet = Array(1, 2, 3, 4, 5, 6, 7, 0))
      dut.io.policy.control.update.poke(true.B)
      dut.io.policy.info.isHit.poke(false.B)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(1.U)
      //      assertNumericalReplacementSet(dut, expectedSet = Array(1, 3, 4, 5, 6, 7, 0, 2))
      dut.io.policy.control.update.poke(true.B)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(3.U)
      //      assertNumericalReplacementSet(dut, expectedSet = Array(3, 4, 5, 6, 7, 0, 1, 2))
      dut.io.policy.control.update.poke(true.B)
      dut.io.policy.info.hitWay.poke(5.U)
      dut.io.policy.info.isHit.poke(true.B)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(4.U)
      //      assertNumericalReplacementSet(dut, expectedSet = Array(4, 5, 6, 7, 0, 1, 2, 3))
      dut.io.policy.control.update.poke(true.B)
      dut.io.policy.info.hitWay.poke(5.U)
      dut.io.policy.info.isHit.poke(true.B)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(4.U)
      //      assertNumericalReplacementSet(dut, expectedSet = Array(4, 6, 7, 0, 1, 2, 3, 5))
      dut.io.policy.control.update.poke(true.B)
      dut.io.policy.info.hitWay.poke(7.U)
      dut.io.policy.info.isHit.poke(true.B)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(4.U)
      //      assertNumericalReplacementSet(dut, expectedSet = Array(4, 6, 7, 0, 1, 2, 3, 5))
      dut.io.policy.control.update.poke(true.B)
      dut.io.policy.info.isHit.poke(false.B)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(4.U)
      //      assertNumericalReplacementSet(dut, expectedSet = Array(4, 6, 0, 1, 2, 3, 5, 7))
      dut.io.policy.control.update.poke(true.B)
      dut.io.policy.control.setIdx.poke(1.U)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(6.U)
      //      assertNumericalReplacementSet(dut, expectedSet = Array(6, 0, 1, 2, 3, 4, 5, 7))
      dut.io.policy.control.update.poke(true.B)
      dut.io.policy.control.setIdx.poke(0.U)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(0.U)
      //      assertNumericalReplacementSet(dut, expectedSet = Array(0, 1, 2, 3, 4, 5, 7, 6))
      dut.io.policy.control.update.poke(false.B)
      dut.io.policy.control.setIdx.poke(1.U)

      //------------- set 1
      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(0.U)
      //      assertNumericalReplacementSet(dut, expectedSet = Array(0, 1, 2, 3, 4, 5, 6, 7))
      dut.io.policy.control.update.poke(true.B)

      //------------- set 0
      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(0.U)
      //      assertNumericalReplacementSet(dut, expectedSet = Array(0, 1, 2, 3, 4, 5, 7, 6))
      dut.io.policy.control.update.poke(true.B)

      //------------- set 1
      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(1.U)
      //      assertNumericalReplacementSet(dut, expectedSet = Array(1, 2, 3, 4, 5, 6, 7, 0))
      dut.io.policy.control.update.poke(true.B)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(2.U)
      //      assertNumericalReplacementSet(dut, expectedSet = Array(1, 2, 3, 4, 5, 7, 0, 6))
      dut.io.policy.control.update.poke(true.B)

      dut.clock.step()
    }
  }
}