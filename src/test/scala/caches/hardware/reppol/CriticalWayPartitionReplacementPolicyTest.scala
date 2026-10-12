package caches.hardware.reppol

import caches.hardware.reppol.LeastCriticalReplacementPolicyTest._
import caches.hardware.reppol.ReplacementPolicyTest._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CriticalWayPartitionReplacementPolicyTest extends AnyFlatSpec with ChiselScalatestTester {
  // Four ways: by default ways 0 and 1 are critical, ways 2 and 3 non-critical
  val (nWays, nSets, nCores) = (4, 2, 4)

  def partitionPolicy() = new PolicyTestWrapper(() => new CriticalWayPartitionReplacementPolicy(nWays, nSets, nCores))

  "CriticalWayPartitionReplacementPolicy" should "use only the non-critical ways when no core is critical" in {
    test(partitionPolicy()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      assert(runSequential(dut, Seq.fill(4)(miss(1))) == Seq(2, 3, 2, 3))
      assert(runSequential(dut, Seq.fill(2)(miss(0))) == Seq(2, 3))
    }
  }

  "CriticalWayPartitionReplacementPolicy" should "keep critical and non-critical cores in their partitions" in {
    test(partitionPolicy()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      setCoreAsCritical(dut, coreID = 0, wData = 2)

      // Interleaved misses: each core cycles through its own partition in least recently used order
      assert(runSequential(dut, Seq(miss(0), miss(1), miss(0), miss(1), miss(0), miss(1))) == Seq(0, 2, 1, 3, 0, 2))

      // Many non-critical misses never evict a critical way
      assert(runSequential(dut, Seq.fill(6)(miss(2))).forall(way => way >= 2))
      assert(runSequential(dut, Seq.fill(2)(miss(0))) == Seq(1, 0))
    }
  }

  "CriticalWayPartitionReplacementPolicy" should "serve hits from any way and only reorder the partition of the hit way" in {
    test(partitionPolicy()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      setCoreAsCritical(dut, coreID = 0, wData = 2)
      assert(runSequential(dut, Seq(miss(0), miss(0))) == Seq(0, 1))

      // A non-critical core hits way 0, which makes way 0 the most recently used critical way
      runSequential(dut, Seq(hit(1, hitWay = 0)))
      assert(runSequential(dut, Seq(miss(0))) == Seq(1))

      // The non-critical partition was not touched by the critical accesses
      assert(runSequential(dut, Seq(miss(1))) == Seq(2))
    }
  }

  "CriticalWayPartitionReplacementPolicy" should "change the split with the value written by the scheduler" in {
    test(partitionPolicy()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Three critical ways: ways 0 to 2 for core 0, only way 3 for the others
      setCoreAsCritical(dut, coreID = 0, wData = 3)
      assert(runSequential(dut, Seq(miss(0), miss(0), miss(0), miss(0))) == Seq(0, 1, 2, 0))
      assert(runSequential(dut, Seq.fill(3)(miss(1))) == Seq(3, 3, 3))

      // Zero keeps the split, values of nWays or more are clamped to nWays-1
      setCoreAsCritical(dut, coreID = 1, wData = 0)
      assert(runSequential(dut, Seq(miss(1))) == Seq(1))
      setCoreAsCritical(dut, coreID = 0, wData = 9)
      assert(runSequential(dut, Seq.fill(2)(miss(2))) == Seq(3, 3))

      // One critical way
      setCoreAsCritical(dut, coreID = 0, wData = 1)
      assert(runSequential(dut, Seq(miss(0), miss(0))) == Seq(0, 0))
    }
  }

  "CriticalWayPartitionReplacementPolicy" should "unset a core on a read" in {
    test(partitionPolicy()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      setCoreAsCritical(dut, coreID = 0, wData = 2)

      dut.io.policy.scheduler.cmd.poke(SchedulerCmd.RD)
      dut.io.policy.scheduler.addr.poke(0.U)
      dut.io.policy.scheduler.rData.expect(1.U)
      dut.clock.step()
      dut.io.policy.scheduler.cmd.poke(SchedulerCmd.NULL)

      // Core 0 is non-critical again and uses the non-critical ways
      assert(runSequential(dut, Seq(miss(0), miss(0))) == Seq(2, 3))
    }
  }

  "CriticalWayPartitionReplacementPolicy" should "forward the bit PLRU state to back-to-back requests" in {
    test(partitionPolicy()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      setCoreAsCritical(dut, coreID = 0, wData = 2)

      // Requests one cycle apart must see the state written by the request ahead of them
      assert(runPipelined(dut, Seq(miss(1), miss(1), miss(1), miss(1))) == Seq(2, 3, 2, 3))
      // Way 3 was used last in the non-critical partition, so core 1 continues with way 2
      assert(runPipelined(dut, Seq(miss(0), miss(1), miss(0), miss(1))) == Seq(0, 2, 1, 3))
    }
  }
}
