package caches.hardware.pipelined

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

case class ExpectedCmd(reqId: Int, coreId: Int, blockOffset: Int)

case class ExpectedMshrStatus(tags: Array[String], indexes: Array[String], validMSHRs: Array[Boolean], fullSignals: Array[Boolean])

class MissFifoTest extends AnyFlatSpec with ChiselScalatestTester {
  def defaultAssignments(dut: MissFifo): Unit = {
    dut.io.push.pushReq.poke(false.B)
    dut.io.pushCrit.poke(false.B)
    dut.io.push.pushReqEntry.tag.poke(0.U)
    dut.io.push.pushReqEntry.index.poke(0.U)
    dut.io.push.pushReqEntry.byteEn.poke(0.U) // All bytes disabled
    dut.io.push.pushReqEntry.replaceWay.poke(0.U)
    dut.io.push.pushCmd.poke(false.B)
    dut.io.push.mshrIdx.poke(0.U)
    dut.io.push.pushCmdEntry.reqId.poke(0.U)
    dut.io.push.pushCmdEntry.coreId.poke(0.U)
    dut.io.push.pushCmdEntry.blockOffset.poke(0.U)
    dut.io.push.updateByteEn.poke(0.U)
    dut.io.push.updateByteEnRow.poke(0.U)
    dut.io.push.updateByteEnCol.poke(0.U)
    dut.io.push.updateByteEnVal.poke(0.U)
    dut.io.pop.pop.poke(false.B)
  }

  def pushCmd(dut: MissFifo, reqId: Int, coreId: Int, blockOff: Int, mshrIdx: Int = 0): Unit = {
    dut.io.push.pushCmd.poke(true.B)
    dut.io.push.mshrIdx.poke(mshrIdx.U)
    dut.io.push.pushCmdEntry.reqId.poke(reqId.U)
    dut.io.push.pushCmdEntry.coreId.poke(coreId.U)
    dut.io.push.pushCmdEntry.blockOffset.poke(blockOff.U)
  }

  def pushReq(
               dut: MissFifo,
               tag: Int,
               index: Int,
               byteEn: Int,
               repWay: Int,
               reqId: Int = 0,
               coreId: Int = 0,
               blockOff: Int = 0
             ): Unit = {
    // Push an entry
    dut.io.push.pushReq.poke(true.B)
    dut.io.push.pushReqEntry.tag.poke(tag.U)
    dut.io.push.pushReqEntry.index.poke(index.U)
    dut.io.push.pushReqEntry.byteEn.poke(byteEn.U)
    dut.io.push.pushReqEntry.replaceWay.poke(repWay.U)

    dut.io.push.pushCmdEntry.reqId.poke(reqId.U)
    dut.io.push.pushCmdEntry.coreId.poke(coreId.U)
    dut.io.push.pushCmdEntry.blockOffset.poke(blockOff.U)
  }

  def updateByteEn(dut: MissFifo, row: Int, col: Int, byteEn: Int): Unit = {
    dut.io.push.updateByteEn.poke(true.B)
    dut.io.push.updateByteEnRow.poke(row.U)
    dut.io.push.updateByteEnCol.poke(col.U)
    dut.io.push.updateByteEnVal.poke(byteEn.U)
  }

  def assertFifoCapacity(dut: MissFifo, full: Boolean, empty: Boolean, crit: Boolean = false): Unit = {
    dut.io.full.expect(full.B)
    if (crit) {
      dut.io.critEmpty.expect(empty.B)
    } else {
      dut.io.nonCritEmpty.expect(empty.B)
    }
  }

  def popEntry(dut: MissFifo, tag: Int, index: Int, byteEn: Int, repWay: Int, cmdCnt: Int): Unit = {
    dut.io.pop.pop.poke(true.B)
    dut.io.pop.popEntry.tag.expect(tag.U)
    dut.io.pop.popEntry.index.expect(index.U)
    dut.io.pop.popEntry.byteEn.expect(byteEn.U)
    dut.io.pop.popEntry.replaceWay.expect(repWay.U)
    dut.io.pop.cmdCnt.expect(cmdCnt.U)
  }

  def assertMshrStatusCritQueue(dut: MissFifo, crit: Boolean, expectedStatus: ExpectedMshrStatus): Unit = {
    if (crit) {
      val currentTags = dut.io.critInfo.currentTags
      val currentIndexes = dut.io.critInfo.currentIndexes
      val validMSHRs = dut.io.critInfo.validMshrs
      val fullSignals = dut.io.critInfo.fullCmds

      for (i <- expectedStatus.indexes.indices) {
        currentTags(i).expect(expectedStatus.tags(i).U)
        currentIndexes(i).expect(expectedStatus.indexes(i).U)
        validMSHRs(i).expect(expectedStatus.validMSHRs(i).B)
        fullSignals(i).expect(expectedStatus.fullSignals(i).B)
      }
    } else {
      val currentTags = dut.io.nonCritInfo.currentTags
      val currentIndexes = dut.io.nonCritInfo.currentIndexes
      val validMSHRs = dut.io.nonCritInfo.validMshrs
      val fullSignals = dut.io.nonCritInfo.fullCmds

      for (i <- expectedStatus.indexes.indices) {
        currentTags(i).expect(expectedStatus.tags(i).U)
        currentIndexes(i).expect(expectedStatus.indexes(i).U)
        validMSHRs(i).expect(expectedStatus.validMSHRs(i).B)
        fullSignals(i).expect(expectedStatus.fullSignals(i).B)
      }
    }
  }

  def assertCmds(dut: MissFifo, expectedCmds: Array[Option[ExpectedCmd]], print: Boolean = false): Unit = {
    println("Commands in MissFifo:")
    val cmdSignals = dut.io.pop.cmds

    for (i <- 0 until cmdSignals.length) {
      val expectedCmd = expectedCmds(i)

      if (expectedCmd.isDefined) {
        val expectedCmdVal = expectedCmd.get

        cmdSignals(i).reqId.expect(expectedCmdVal.reqId.U)
        cmdSignals(i).coreId.expect(expectedCmdVal.coreId.U)
        cmdSignals(i).blockOffset.expect(expectedCmdVal.blockOffset.U)
      } else {
        cmdSignals(i).reqId.expect(0.U)
        cmdSignals(i).coreId.expect(0.U)
        cmdSignals(i).blockOffset.expect(0.U)
      }

      println(s"Cmd $i: " +
        s"reqId=${cmdSignals(i).reqId.peekInt()}, " +
        s"coreId=${cmdSignals(i).coreId.peekInt()}, " +
        s"blockOffset=${cmdSignals(i).blockOffset.peekInt()}."
      )
    }

    println("")
  }

  "MissFifo" should "push and pop entries correctly for non critical queue only" in {
    val blockWidth = 64
    val subBlockWidth = 16

    test(new MissFifo(
      nCores = 4,
      nCmds = 4,
      nMshrs = 4,
      nWays = 16,
      reqIdWidth = 16,
      tagWidth = 8,
      indexWidth = 4,
      blockOffsetWidth = 2,
      subBlockWidth = subBlockWidth,
      blockWidth = blockWidth
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Initialize inputs
      defaultAssignments(dut)

      // Push an entry
      pushReq(dut, tag = 0x12, index = 0x2, byteEn = 0x3, repWay = 2, reqId = 1, coreId = 2)
      assertFifoCapacity(dut, full = false, empty = true)

      dut.clock.step()

      // Push an entry
      pushReq(dut, tag = 0x44, index = 0x1, byteEn = 0x1, repWay = 3, reqId = 2, coreId = 2)
      assertFifoCapacity(dut, full = false, empty = false)

      dut.clock.step()

      // Push an entry
      pushReq(dut, tag = 0xaa, index = 0xc, byteEn = 0x2, repWay = 2, reqId = 3, coreId = 1, blockOff = 2)
      assertFifoCapacity(dut, full = false, empty = false)

      dut.clock.step()

      // Push an entry
      pushReq(dut, tag = 0xbb, index = 0xe, byteEn = 0x0, repWay = 1, reqId = 4, coreId = 3, blockOff = 3)
      assertFifoCapacity(dut, full = false, empty = false)

      dut.clock.step()

      defaultAssignments(dut)

      // Add more commands to the first fifo entry
      pushCmd(dut, reqId = 11, coreId = 3, blockOff = 3)

      dut.clock.step()

      pushCmd(dut, reqId = 13, coreId = 1, blockOff = 0)

      dut.clock.step()

      pushCmd(dut, reqId = 5, coreId = 2, blockOff = 2)

      dut.clock.step()

      defaultAssignments(dut)

      // Pop an entry
      popEntry(dut, tag = 0x12, index = 0x2, byteEn = 0x3, repWay = 2, cmdCnt = 4)
      assertFifoCapacity(dut, full = true, empty = false)

      dut.io.nonCritInfo.fullCmds(0).expect(true.B)

      assertCmds(dut, Array(Some(ExpectedCmd(1, 2, 0)), Some(ExpectedCmd(11, 3, 3)), Some(ExpectedCmd(13, 1, 0)), Some(ExpectedCmd(5, 2, 2))), print = true)

      assertMshrStatusCritQueue(
        dut,
        crit = false,
        ExpectedMshrStatus(
          tags = Array("h12", "h44", "haa", "hbb"),
          indexes = Array("h2", "h1", "hc", "he"),
          validMSHRs = Array(true, true, true, true),
          fullSignals = Array(true, false, false, false)
        )
      )

      dut.clock.step()

      dut.io.pop.pop.poke(false.B)

      // Update the byte enable for the second entry
      updateByteEn(dut, row = 1, col = 1, byteEn = 0x2)

      dut.clock.step()

      // Update the byte enable for the second entry again
      updateByteEn(dut, row = 1, col = 2, byteEn = 0x1)

      dut.clock.step()

      defaultAssignments(dut)

      // Add more commands to the second fifo entry
      pushCmd(dut, reqId = 7, coreId = 2, blockOff = 1, mshrIdx = 1)

      dut.clock.step()

      defaultAssignments(dut)

      // Pop an entry
      popEntry(dut, tag = 0x44, index = 0x1, byteEn = 0x19, repWay = 3, cmdCnt = 2)
      assertFifoCapacity(dut, full = false, empty = false)

      dut.io.nonCritInfo.fullCmds(1).expect(false.B)

      assertCmds(dut, Array(Some(ExpectedCmd(2, 2, 0)), Some(ExpectedCmd(7, 2, 1)), None, None), print = true)

      assertMshrStatusCritQueue(
        dut,
        crit = false,
        ExpectedMshrStatus(
          tags = Array("h12", "h44", "haa", "hbb"),
          indexes = Array("h2", "h1", "hc", "he"),
          validMSHRs = Array(false, true, true, true),
          fullSignals = Array(true, false, false, false)
        )
      )

      dut.clock.step()

      // Pop an entry
      popEntry(dut, tag = 0xaa, index = 0xc, byteEn = 0x20, repWay = 2, cmdCnt = 1)
      assertFifoCapacity(dut, full = false, empty = false)

      dut.io.nonCritInfo.fullCmds(2).expect(false.B)

      assertCmds(dut, Array(Some(ExpectedCmd(3, 1, 2)), None, None, None), print = true)

      assertMshrStatusCritQueue(
        dut,
        crit = false,
        ExpectedMshrStatus(
          tags = Array("h12", "h44", "haa", "hbb"),
          indexes = Array("h2", "h1", "hc", "he"),
          validMSHRs = Array(false, false, true, true),
          fullSignals = Array(true, false, false, false)
        )
      )

      dut.clock.step()

      // Pop an entry
      popEntry(dut, tag = 0xbb, index = 0xe, byteEn = 0x0, repWay = 1, cmdCnt = 1)
      assertFifoCapacity(dut, full = false, empty = false)

      dut.io.nonCritInfo.fullCmds(3).expect(false.B)

      assertCmds(dut, Array(Option(ExpectedCmd(4, 3, 3)), None, None, None), print = true)

      assertMshrStatusCritQueue(
        dut,
        crit = false,
        ExpectedMshrStatus(
          tags = Array("h12", "h44", "haa", "hbb"),
          indexes = Array("h2", "h1", "hc", "he"),
          validMSHRs = Array(false, false, false, true),
          fullSignals = Array(true, false, false, false)
        )
      )

      dut.clock.step()

      dut.io.push.full.expect(false.B)
      dut.io.pop.empty.expect(true.B)

      assertMshrStatusCritQueue(
        dut,
        crit = false,
        ExpectedMshrStatus(
          tags = Array("h12", "h44", "haa", "hbb"),
          indexes = Array("h2", "h1", "hc", "he"),
          validMSHRs = Array(false, false, false, false),
          fullSignals = Array(true, false, false, false)
        )
      )

      // Push an entry
      pushReq(dut, tag = 0xa, index = 0xb, byteEn = 0x3, repWay = 2, reqId = 12, coreId = 2)

      dut.clock.step()

      defaultAssignments(dut)

      assertMshrStatusCritQueue(
        dut,
        crit = false,
        ExpectedMshrStatus(
          tags = Array("ha", "h44", "haa", "hbb"),
          indexes = Array("hb", "h1", "hc", "he"),
          validMSHRs = Array(true, false, false, false),
          fullSignals = Array(false, false, false, false)
        )
      )

      dut.clock.step()
    }
  }

  "MissFifo" should "push and pop entries correctly for critical and non-critical queues" in {
    val blockWidth = 64
    val subBlockWidth = 16

    test(new MissFifo(
      nCores = 4,
      nCmds = 4,
      nMshrs = 4,
      nWays = 16,
      reqIdWidth = 16,
      tagWidth = 8,
      indexWidth = 4,
      blockOffsetWidth = 2,
      subBlockWidth = subBlockWidth,
      blockWidth = blockWidth,
      enCritMisses = true
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      // Initialize inputs
      defaultAssignments(dut)

      // Push non-critical entry
      pushReq(dut, tag = 0x12, index = 0x2, byteEn = 0x3, repWay = 2, reqId = 1, coreId = 2)
      assertFifoCapacity(dut, full = false, empty = true)

      dut.clock.step()

      // Push critical entry
      dut.io.pushCrit.poke(true.B)
      pushReq(dut, tag = 0x44, index = 0x1, byteEn = 0x1, repWay = 3, reqId = 2, coreId = 2)
      assertFifoCapacity(dut, full = false, empty = false)

      dut.clock.step()

      // Push critical entry
      pushReq(dut, tag = 0xaa, index = 0xc, byteEn = 0x2, repWay = 2, reqId = 3, coreId = 1, blockOff = 2)
      assertFifoCapacity(dut, full = false, empty = false)

      dut.clock.step()

      // Push non-critical entry
      dut.io.pushCrit.poke(false.B)
      pushReq(dut, tag = 0xbb, index = 0xe, byteEn = 0x3, repWay = 1, reqId = 4, coreId = 3, blockOff = 3)
      assertFifoCapacity(dut, full = false, empty = false)

      dut.clock.step()

      defaultAssignments(dut)

      // Add more commands
      pushCmd(dut, reqId = 5, coreId = 3, blockOff = 3)

      dut.clock.step()

      dut.io.pushCrit.poke(true.B)
      pushCmd(dut, reqId = 6, coreId = 1, blockOff = 0)

      dut.clock.step()

      pushCmd(dut, reqId = 7, coreId = 2, blockOff = 2, mshrIdx = 1)

      dut.clock.step()

      dut.io.pushCrit.poke(false.B)
      pushCmd(dut, reqId = 8, coreId = 2, blockOff = 2, mshrIdx = 1)

      dut.clock.step()

      defaultAssignments(dut)

      // Pop an entry from non-critical q
      popEntry(dut, tag = 0x12, index = 0x2, byteEn = 0x3, repWay = 2, cmdCnt = 2)
      assertFifoCapacity(dut, full = false, empty = false)

      assertCmds(dut, Array(Some(ExpectedCmd(1, 2, 0)), Some(ExpectedCmd(5, 3, 3)), None, None), print = true)

      assertMshrStatusCritQueue(
        dut,
        crit = false,
        ExpectedMshrStatus(
          tags = Array("h12", "hbb", "h0", "h0"),
          indexes = Array("h2", "he", "h0", "h0"),
          validMSHRs = Array(true, true, false, false),
          fullSignals = Array(false, false, false, false)
        )
      )

      dut.clock.step()

      // Pop an entry from critical q
      dut.io.popQSel.poke(true.B)
      popEntry(dut, tag = 0x44, index = 0x1, byteEn = 0x1, repWay = 3, cmdCnt = 2)
      assertFifoCapacity(dut, full = false, empty = false)

      assertCmds(dut, Array(Some(ExpectedCmd(2, 2, 0)), Some(ExpectedCmd(6, 1, 0)), None, None), print = true)

      assertMshrStatusCritQueue(
        dut,
        crit = true,
        ExpectedMshrStatus(
          tags = Array("h44", "haa", "h0", "h0"),
          indexes = Array("h1", "hc", "h0", "h0"),
          validMSHRs = Array(true, true, false, false),
          fullSignals = Array(false, false, false, false)
        )
      )

      dut.clock.step()

      defaultAssignments(dut)

      // Push critical entry
      dut.io.pushCrit.poke(true.B)
      pushReq(dut, tag = 0x77, index = 0x4, byteEn = 0x3, repWay = 0, reqId = 9, coreId = 1, blockOff = 1)
      assertFifoCapacity(dut, full = false, empty = false)

      dut.clock.step()

      // Push another critical entry
      pushReq(dut, tag = 0x12, index = 0xf, byteEn = 0x0, repWay = 0, reqId = 10, coreId = 2)
      assertFifoCapacity(dut, full = false, empty = false)

      dut.clock.step()

      // Fill up critical queue with the final entry
      pushReq(dut, tag = 0xc4, index = 0xa, byteEn = 0x1, repWay = 0, reqId = 11, coreId = 1)
      assertFifoCapacity(dut, full = false, empty = false)

      dut.clock.step()

      defaultAssignments(dut)

      // Add more commands the second entry of critical queue
      dut.io.pushCrit.poke(true.B)
      pushCmd(dut, reqId = 12, coreId = 3, blockOff = 3, mshrIdx = 2)

      dut.clock.step()

      pushCmd(dut, reqId = 13, coreId = 1, blockOff = 0, mshrIdx = 2)

      dut.clock.step()

      pushCmd(dut, reqId = 14, coreId = 2, blockOff = 2, mshrIdx = 2)

      dut.clock.step()

      defaultAssignments(dut)

      // Pop an entry from critical q
      dut.io.popQSel.poke(true.B)
      popEntry(dut, tag = 0xaa, index = 0xc, byteEn = 0x20, repWay = 2, cmdCnt = 2)
      assertFifoCapacity(dut, full = true, empty = false, crit = true)

      assertCmds(dut, Array(Some(ExpectedCmd(3, 1, 2)), Some(ExpectedCmd(7, 2, 2)), None, None), print = true)

      assertMshrStatusCritQueue(
        dut,
        crit = true,
        ExpectedMshrStatus(
          tags = Array("hc4", "haa", "h77", "h12"),
          indexes = Array("ha", "hc", "h4", "hf"),
          validMSHRs = Array(true, true, true, true),
          fullSignals = Array(false, false, true, false)
        )
      )

      dut.clock.step()

      // Pop another entry from critical q
      dut.io.popQSel.poke(true.B)
      popEntry(dut, tag = 0x77, index = 0x4, byteEn = 0xc, repWay = 0, cmdCnt = 4)
      assertFifoCapacity(dut, full = false, empty = false, crit = true)

      assertCmds(dut, Array(Some(ExpectedCmd(9, 1, 1)), Some(ExpectedCmd(12, 3, 3)), Some(ExpectedCmd(13, 1, 0)), Some(ExpectedCmd(14, 2, 2))), print = true)

      assertMshrStatusCritQueue(
        dut,
        crit = true,
        ExpectedMshrStatus(
          tags = Array("hc4", "haa", "h77", "h12"),
          indexes = Array("ha", "hc", "h4", "hf"),
          validMSHRs = Array(true, false, true, true),
          fullSignals = Array(false, false, true, false)
        )
      )

      dut.clock.step()

      // Pop non-critical entry
      dut.io.popQSel.poke(false.B)
      popEntry(dut, tag = 0xbb, index = 0xe, byteEn = 0xc0, repWay = 1, cmdCnt = 2)
      assertFifoCapacity(dut, full = false, empty = false)

      assertCmds(dut, Array(Some(ExpectedCmd(4, 3, 3)), Some(ExpectedCmd(8, 2, 2)), None, None), print = true)

      assertMshrStatusCritQueue(
        dut,
        crit = false,
        ExpectedMshrStatus(
          tags = Array("h12", "hbb", "h0", "h0"),
          indexes = Array("h2", "he", "h0", "h0"),
          validMSHRs = Array(false, true, false, false),
          fullSignals = Array(false, false, false, false)
        )
      )

      dut.clock.step()
    }
  }
}
