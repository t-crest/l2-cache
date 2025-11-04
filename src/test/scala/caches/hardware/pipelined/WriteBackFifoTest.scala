package caches.hardware.pipelined

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class WriteBackFifoTest extends AnyFlatSpec with ChiselScalatestTester {
  def setPushEntry(dut: WriteBackFifo, tag: Int, index: Int, wbData: String, isCrit: Boolean, pushToCritQ: Boolean = false): Unit = {
    dut.io.push.push.poke(true.B)
    dut.io.push.pushEntry.tag.poke(tag.U)
    dut.io.push.pushEntry.index.poke(index.U)
    dut.io.push.pushEntry.wbData.poke(wbData.U)
    dut.io.push.pushEntry.isCrit.poke(isCrit.B)
    dut.io.pushCrit.poke(pushToCritQ.B)
  }

  def expectPopEntry(dut: WriteBackFifo, tag: Int, index: Int, wbData: String, isCrit: Boolean): Unit = {
    // Pop an entry
    dut.io.pop.pop.poke(true.B)
    dut.io.pop.popEntry.tag.expect(tag.U)
    dut.io.pop.popEntry.index.expect(index.U)
    dut.io.pop.popEntry.wbData.expect(wbData.U)
  }

  def resetInputs(dut: WriteBackFifo): Unit = {
    dut.io.pushCrit.poke(false.B)
    dut.io.popQSel.poke(false.B)
    dut.io.push.push.poke(false.B)
    dut.io.push.pushEntry.tag.poke(0.U)
    dut.io.push.pushEntry.index.poke(0.U)
    dut.io.push.pushEntry.wbData.poke(0.U)
    dut.io.pop.pop.poke(false.B)
  }

  "WriteBackFifo" should "push and pop entries from non-critical queue only" in {
    test(new WriteBackFifo(queueDepth = 2, tagWidth = 16, indexWidth = 8, blockWidth = 32)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Initialize inputs
      resetInputs(dut)

      // Push an entry
      dut.io.push.push.poke(true.B)
      dut.io.push.pushEntry.tag.poke("h1234".U)
      dut.io.push.pushEntry.index.poke("h12".U)
      dut.io.push.pushEntry.wbData.poke("hdeadbeef".U)

      dut.io.push.full.expect(false.B)
      dut.io.pop.empty.expect(true.B)

      dut.clock.step(1)

      dut.io.push.push.poke(false.B)

      // Pop an entry
      dut.io.pop.pop.poke(true.B)
      dut.io.pop.popEntry.tag.expect("h1234".U)
      dut.io.pop.popEntry.index.expect("h12".U)
      dut.io.pop.popEntry.wbData.expect("hdeadbeef".U)

      dut.clock.step(1)

      dut.io.pop.pop.poke(false.B)
      dut.io.pop.empty.expect(true.B)

      // Push an entry
      dut.io.push.push.poke(true.B)
      dut.io.push.pushEntry.tag.poke("h4321".U)
      dut.io.push.pushEntry.index.poke("h17".U)
      dut.io.push.pushEntry.wbData.poke("hcafebabe".U)

      dut.clock.step(1)

      // Push another entry
      dut.io.push.push.poke(true.B)
      dut.io.push.pushEntry.tag.poke("h5678".U)
      dut.io.push.pushEntry.index.poke("h21".U)
      dut.io.push.pushEntry.wbData.poke("hbabecafe".U)

      dut.clock.step(1)

      dut.io.push.push.poke(false.B)
      dut.io.push.full.expect(true.B)

      dut.io.pop.pop.poke(true.B)
      dut.io.pop.popEntry.tag.expect("h4321".U)
      dut.io.pop.popEntry.index.expect("h17".U)
      dut.io.pop.popEntry.wbData.expect("hcafebabe".U)

      dut.clock.step(1)

      dut.io.pop.pop.poke(true.B)
      dut.io.pop.popEntry.tag.expect("h5678".U)
      dut.io.pop.popEntry.index.expect("h21".U)
      dut.io.pop.popEntry.wbData.expect("hbabecafe".U)

      dut.clock.step(1)

      dut.io.pop.pop.poke(false.B)
      dut.io.pop.empty.expect(true.B)

      dut.clock.step(1)
    }
  }

  "WriteBackFifo" should "switch between critical and non-critical wb Qs" in {
    test(new WriteBackFifo(queueDepth = 4, tagWidth = 16, indexWidth = 8, blockWidth = 32, enCritWb = true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Initialize inputs
      resetInputs(dut)

      // ------ Push and pop a single non-critical entry to Q
      dut.clock.step(1)

      resetInputs(dut)

      setPushEntry(dut, tag = 0x1234, index = 0x12, wbData = "hdeadbeef", isCrit = false)

      dut.io.push.full.expect(false.B)
      dut.io.nonCritEmpty.expect(true.B)

      dut.clock.step(1)

      resetInputs(dut)

      expectPopEntry(dut, tag = 0x1234, index = 0x12, wbData = "hdeadbeef", isCrit = false)

      dut.io.push.full.expect(false.B)
      dut.io.nonCritEmpty.expect(false.B)

      // ------ Push two non-critical entries to Q
      dut.clock.step(1)

      resetInputs(dut)

      dut.io.push.full.expect(false.B)
      dut.io.nonCritEmpty.expect(true.B)

      setPushEntry(dut, tag = 0x4321, index = 0x17, wbData = "hcafebabe", isCrit = false)

      dut.clock.step(1)

      resetInputs(dut)

      setPushEntry(dut, tag = 0x5678, index = 0x21, wbData = "hbabecafe", isCrit = false)

      dut.io.push.full.expect(false.B)
      dut.io.nonCritEmpty.expect(false.B)

      // ------ Push a critical entry to Q and pop non-critical entry
      dut.clock.step(1)

      resetInputs(dut)

      setPushEntry(dut, tag = 0x7210, index = 0x05, wbData = "hdeadbabe", isCrit = true)

      expectPopEntry(dut, tag = 0x4321, index = 0x17, wbData = "hcafebabe", isCrit = false)

      dut.io.push.full.expect(false.B)
      dut.io.nonCritEmpty.expect(false.B)
      dut.io.critEmpty.expect(true.B)

      // ------ Push a critical entry to critical-Q
      dut.clock.step(1)

      resetInputs(dut)

      setPushEntry(dut, tag = 0x3150, index = 0x74, wbData = "hdeadc0de", isCrit = true, pushToCritQ = true)

      dut.io.nonCritEmpty.expect(false.B)
      dut.io.critEmpty.expect(true.B)

      // ------ Pop an entry from critical queue and push a non-critical entry
      dut.clock.step(1)

      resetInputs(dut)

      // Choose the critical Q to pop from
      dut.io.popQSel.poke(1.U)

      setPushEntry(dut, tag = 0x0010, index = 0x05, wbData = "h00d15ea5", isCrit = false)

      expectPopEntry(dut, tag = 0x3150, index = 0x74, wbData = "hdeadc0de", isCrit = true)

      dut.io.nonCritEmpty.expect(false.B)
      dut.io.critEmpty.expect(false.B)

      // --- Force the wb queue to switch back to non-critical Q
      dut.clock.step(1)

      resetInputs(dut)

      // Choose the non-critical Q to pop from
      dut.io.popQSel.poke(0.U)

      expectPopEntry(dut, tag = 0x5678, index = 0x21, wbData = "hbabecafe", isCrit = false)

      dut.io.nonCritEmpty.expect(false.B)
      dut.io.critEmpty.expect(true.B)

      // ------ Pop a critical entry from non-critical queue
      dut.clock.step(1)

      resetInputs(dut)

      expectPopEntry(dut, tag = 0x7210, index = 0x05, wbData = "hdeadbabe", isCrit = true)

      dut.io.nonCritEmpty.expect(false.B)
      dut.io.critEmpty.expect(true.B)

      // ------ Pop the final non-critical entry from the non-critical queue
      dut.clock.step(1)

      resetInputs(dut)

      expectPopEntry(dut, tag = 0x0010, index = 0x05, wbData = "h00d15ea5", isCrit = false)

      dut.io.nonCritEmpty.expect(false.B)
      dut.io.critEmpty.expect(true.B)

      dut.clock.step(1)

      dut.io.nonCritEmpty.expect(true.B)
      dut.io.critEmpty.expect(true.B)

      dut.clock.step(1)
    }
  }
}
