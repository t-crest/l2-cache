package caches.hardware.reppol

import caches.hardware.reppol.ReplacementPolicyTest._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class TimeoutReplacementPolicyTest extends AnyFlatSpec with ChiselScalatestTester {
  "TimeoutReplacementPolicy" should "Without critical uses PLRU" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    val polGen = () => new TimeoutReplacementPolicy(nWays, nSets, nCores, BasePolicies.BIT_PLRU, repSetFormat = new MruFormat)
    test(new PolicyTestWrapper(polGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val workingSet = 1
      dut.clock.step()

      cancel("timeout policy needs to be adjusted to new cache pipeline format")

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 3)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 3)
    }
  }

  "TimeoutReplacementPolicy" should "Reject non-critical when all reserved" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    val polGen = () => new TimeoutReplacementPolicy(nWays, nSets, nCores, BasePolicies.BIT_PLRU, repSetFormat = new MruFormat)
    test(new PolicyTestWrapper(polGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val workingSet = 1
      dut.clock.step()

      cancel("timeout policy needs to be adjusted to new cache pipeline format")

      // Set the first core as critical with a long timeout
      val longTimeout = 1023
      setCoreAsCritical(dut, coreID = 0, wData = longTimeout)

      // Assign all ways to critical
      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 3)

      dut.clock.step(Random.between(1, longTimeout / 2))

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = None)
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = None)
    }
  }

  "TimeoutReplacementPolicy" should "Critical may evict critical using PLRU" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    val polGen = () => new TimeoutReplacementPolicy(nWays, nSets, nCores, BasePolicies.BIT_PLRU, repSetFormat = new MruFormat)
    test(new PolicyTestWrapper(polGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val workingSet = 1
      dut.clock.step()

      cancel("timeout policy needs to be adjusted to new cache pipeline format")

      setCoreAsCritical(dut, coreID = 0, wData = 1023)
      setCoreAsCritical(dut, coreID = 1, wData = 1023)

      // Assign all ways to critical
      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 3)

      // use another critical to evict
      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 1)
    }
  }

  "TimeoutReplacementPolicy" should "Critical prioritized timed out" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    val polGen = () => new TimeoutReplacementPolicy(nWays, nSets, nCores, BasePolicies.BIT_PLRU, repSetFormat = new MruFormat)
    test(new PolicyTestWrapper(polGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val workingSet = 1
      dut.clock.step()

      cancel("timeout policy needs to be adjusted to new cache pipeline format")

      setCoreAsCritical(dut, coreID = 0, wData = 1023)
      setCoreAsCritical(dut, coreID = 1, wData = 1023)

      // Assign all ways to critical except two
      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 3)

      // Use another critical to evict
      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 3)
    }
  }

  "TimeoutReplacementPolicy" should "Non-critical can evict timed out" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    val polGen = () => new TimeoutReplacementPolicy(nWays, nSets, nCores, BasePolicies.BIT_PLRU, repSetFormat = new MruFormat)
    test(new PolicyTestWrapper(polGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val workingSet = 1
      dut.clock.step()

      cancel("timeout policy needs to be adjusted to new cache pipeline format")

      // Set the first core as critical with a long timeout
      setCoreAsCritical(dut, coreID = 0, wData = 1023)

      // Assign all ways to critical except two
      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 3)

      // Use another critical to evict
      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 2)
    }
  }

  "TimeoutReplacementPolicy" should "Eventually time out" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    val polGen = () => new TimeoutReplacementPolicy(nWays, nSets, nCores, BasePolicies.BIT_PLRU, repSetFormat = new MruFormat)
    test(new PolicyTestWrapper(polGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val workingSet = 1
      dut.clock.step()

      cancel("timeout policy needs to be adjusted to new cache pipeline format")

      // Set the first core as critical with a long timeout
      setCoreAsCritical(dut, coreID = 2, wData = 500)

      // Assign all ways to critical
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 3)

      // Tick enough to ensure that all sets have timed out
      dut.clock.setTimeout(1200)
      dut.clock.step(1000)

      // Non-critical should now be able to evict
      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 3)
    }
  }

  "TimeoutReplacementPolicy" should "Update refreshes timeout" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    val polGen = () => new TimeoutReplacementPolicy(nWays, nSets, nCores, BasePolicies.BIT_PLRU, repSetFormat = new MruFormat)
    test(new PolicyTestWrapper(polGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val workingSet = 1
      dut.clock.step()

      cancel("timeout policy needs to be adjusted to new cache pipeline format")

      val timeout = 100
      // Set the first core as critical with a short
      setCoreAsCritical(dut, coreID = 2, wData = timeout)

      // Assign all ways to critical
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 3)

      // Tick halfway through the timeout
      dut.clock.step(timeout)
      // Hit a way to refresh timer
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 1)
      // Tick enough to ensure original timers run out
      dut.clock.step(timeout)

      // Non-critical should now be able to evict all except the refreshed
      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1)
    }
  }

  "TimeoutReplacementPolicy" should "Critical with short timeout doesn't refresh long timeout" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    val polGen = () => new TimeoutReplacementPolicy(nWays, nSets, nCores, BasePolicies.BIT_PLRU, repSetFormat = new MruFormat)
    test(new PolicyTestWrapper(polGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val workingSet = 1
      dut.clock.step()

      cancel("timeout policy needs to be adjusted to new cache pipeline format")

      val longTimeout = 1000
      val shortTimeout = 100
      // Set the first core as critical with a long timeout
      setCoreAsCritical(dut, coreID = 2, wData = longTimeout)
      setCoreAsCritical(dut, coreID = 1, wData = shortTimeout)

      // Assign all ways to critical with long timeout
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 3)

      // Hit a way with short timeout core
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 0)

      // Tick enough to pass short timeout
      dut.clock.step(shortTimeout * 2)

      // Non-critical should still not be able to evict
      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = None)
    }
  }

  "TimeoutReplacementPolicy" should "Critical with long timeout refreshes short timeout" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    val polGen = () => new TimeoutReplacementPolicy(nWays, nSets, nCores, BasePolicies.BIT_PLRU, repSetFormat = new MruFormat)
    test(new PolicyTestWrapper(polGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val workingSet = 1
      dut.clock.step()

      cancel("timeout policy needs to be adjusted to new cache pipeline format")

      val longTimeout = 1000
      val shortTimeout = 100
      // Set the first core as critical with a long timeout
      setCoreAsCritical(dut, coreID = 1, wData = longTimeout)
      setCoreAsCritical(dut, coreID = 2, wData = shortTimeout)

      // Assign all ways to critical with short timeout
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 3)

      // Hit a way with long timeout core
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 1)

      // Tick enough to pass short timeout
      dut.clock.step(shortTimeout * 2)

      // Non-critical should be able to evict short timeouts
      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)
      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 2)
      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)
      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 3)
    }
  }
}
