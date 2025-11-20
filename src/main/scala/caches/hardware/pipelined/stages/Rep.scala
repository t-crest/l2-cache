package caches.hardware.pipelined.stages

import chisel3._
import chisel3.util._
import caches.hardware.util._
import caches.hardware.reppol._
import caches.hardware.pipelined.{MshrInfoIO, MshrPushIO, RejectionQueueEntry, WbFifoInfoIO}

class MshrInfoCheck(tagWidth: Int, nWays: Int, nSets: Int, nMshrs: Int) extends Module() {
  val io = IO(new Bundle {
    val idx = Input(UInt(log2Up(nSets).W))
    val tag = Input(UInt(tagWidth.W))
    val reqHit = Input(Bool())
    val repWay = Input(UInt(log2Up(nWays).W))
    val reqValid = Input(Bool())
    val nonCritQIdxs = Input(Vec(nMshrs, UInt(log2Up(nSets).W)))
    val critQIdxs = Input(Vec(nMshrs, UInt(log2Up(nSets).W)))
    val nonCritQTags = Input(Vec(nMshrs, UInt(tagWidth.W)))
    val critQTags = Input(Vec(nMshrs, UInt(tagWidth.W)))
    val nonCritQValid = Input(Vec(nMshrs, Bool()))
    val critQValid = Input(Vec(nMshrs, Bool()))
    val nonCritQCritCores = Input(Vec(nMshrs, Bool()))
    val wbQValid = Input(Vec(nMshrs, Bool()))
    val wbQCritCores = Input(Vec(nMshrs, Bool()))
    val nonCritQRepWays = Input(Vec(nMshrs, UInt(log2Up(nWays).W)))
    val critQRepWays = Input(Vec(nMshrs, UInt(log2Up(nWays).W)))
    val nonCritHalfMissFull = Input(Vec(nMshrs, Bool()))
    val critHalfMissFull = Input(Vec(nMshrs, Bool()))
    val halfMiss = Output(Bool())
    val halfMissCrit = Output(Bool())
    val halfMissIdx = Output(UInt(log2Up(nWays).W))
    val halfMissRepWay = Output(UInt(log2Up(nWays).W))
    val reservedRepWay = Output(Bool())
    val halfMissCapacity = Output(Bool())
    val nonCritMisses = Output(UInt(log2Up(nMshrs).W))
    val nonCritWbs = Output(UInt(log2Up(nMshrs).W))
  })

  val nonCritMissMatches = Wire(Vec(nMshrs, Bool()))
  val critMissMatches = Wire(Vec(nMshrs, Bool()))
  val nonCritMisses = Wire(Vec(nMshrs, Bool()))
  val nonCritWbs = Wire(Vec(nMshrs, Bool()))
  val repWayMatches = Wire(Vec(nMshrs, Bool()))

  for (mshr <- 0 until nMshrs) {
    val nonCritQIdxMatch = io.nonCritQIdxs(mshr) === io.idx
    val nonCritQValid = io.nonCritQValid(mshr)

    nonCritMisses(mshr) := nonCritQValid && io.nonCritQCritCores(mshr)
    nonCritWbs(mshr) := io.wbQValid(mshr) && io.wbQCritCores(mshr)
    repWayMatches(mshr) := nonCritQIdxMatch && io.nonCritQRepWays(mshr) === io.repWay && nonCritQValid
    nonCritMissMatches(mshr) := nonCritQIdxMatch && io.nonCritQTags(mshr) === io.tag && nonCritQValid
    critMissMatches(mshr) := io.critQIdxs(mshr) === io.idx && io.critQTags(mshr) === io.tag && io.critQValid(mshr)
  }

  val anyNonCritMisses = nonCritMissMatches.reduce((x, y) => x || y)
  val anyCritMisses = critMissMatches.reduce((x, y) => x || y)
  val nonCritMissIdx = PriorityEncoder(nonCritMissMatches)
  val critMissIdx = PriorityEncoder(critMissMatches)
  val halfMiss = io.reqValid && anyNonCritMisses || anyCritMisses
  val halfMissIdx = Mux(anyCritMisses, critMissIdx, nonCritMissIdx)
  val reservedRepWay = io.reqValid && !(anyNonCritMisses || anyCritMisses) && !io.reqHit && repWayMatches.reduce((x, y) => x || y)

  // If a single mshr register is full of commands then we stall the pipeline until the line is brought in
  val cmdCapacityNonCrit = anyNonCritMisses && io.nonCritHalfMissFull(halfMissIdx)
  val cmdCapacityCrit = anyCritMisses && io.critHalfMissFull(halfMissIdx)

  io.halfMiss := halfMiss
  io.halfMissCrit := anyCritMisses
  io.halfMissIdx := halfMissIdx
  io.halfMissRepWay := Mux(anyCritMisses, io.critQRepWays(halfMissIdx), io.nonCritQRepWays(halfMissIdx))
  io.reservedRepWay := reservedRepWay
  io.halfMissCapacity := io.reqValid && (cmdCapacityCrit || cmdCapacityNonCrit)
  io.nonCritMisses := PopCount(nonCritMisses)
  io.nonCritWbs := PopCount(nonCritWbs)
}

case class IsHitResult(hit: Bool, hitIdx: UInt)

case class NewTagsAndDirtyBits(dirty: Vec[Bool], tags: Vec[UInt])

class InvalidateLineIO(nWays: Int, indexWidth: Int) extends Bundle {
  val invalidate = Output(Bool())
  val way = Output(UInt(log2Up(nWays).W))
  val index = Output(UInt(indexWidth.W))
}

class RepIO(nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffWidth: Int, subBlockWidth: Int) extends Bundle() {
  val coreId = Input(UInt(log2Up(nCores).W))
  val reqValid = Input(Bool())
  val reqId = Input(UInt(reqIdWidth.W))
  val reqHit = Input(Bool())
  val reqHitWay = Input(UInt(log2Up(nWays).W))
  val reqRw = Input(Bool())
  val wData = Input(UInt(subBlockWidth.W))
  val byteEn = Input(UInt((subBlockWidth / 8).W))
  val dirtyBits = Input(Vec(nWays, Bool()))
  val setTags = Input(Vec(nWays, UInt(tagWidth.W)))
  val blockOffset = Input(UInt(blockOffWidth.W))
  val index = Input(UInt(indexWidth.W))
  val repPolReadIndex = Input(UInt(indexWidth.W))
  val tag = Input(UInt(tagWidth.W))
}

class RepTopIO(nCores: Int, nSets: Int, nWays: Int, nMshrs: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Bundle {
  private val blockOffWidth = log2Up(blockWidth / subBlockWidth)
  private val addrWidth = tagWidth + indexWidth + blockOffWidth + log2Up(subBlockWidth / 8)

  val stall = Input(Bool())
  // Input from previous stage
  val rep = new RepIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffWidth, subBlockWidth)
  // Info signals from miss and wb queues
  val missNonCritInfo = Flipped(new MshrInfoIO(nCores, nMshrs, nWays, indexWidth, tagWidth))
  val missCritInfo = Flipped(new MshrInfoIO(nCores, nMshrs, nWays, indexWidth, tagWidth))
  // Connection to replacement policy
  val repPolCtrl = Flipped(new ReplacementPolicyControlIO(nCores, nWays, nSets))
  val repPolInfo = Flipped(new ReplacementPolicyInfoIO(nWays, nMshrs))
  // Valid and dirty control forwarding signals
  val setLineValid = Flipped(new SetLineValidIO(nWays, indexWidth, tagWidth))
  val dirtyCtrl = Flipped(new DirtyCtrlIO(nWays, indexWidth))
  // Connection to rejection queue
  val pushReject = Output(Bool())
  val pushRejectEntry = Flipped(new RejectionQueueEntry(nCores, addrWidth, subBlockWidth, reqIdWidth))
  // Line invalidation control
  val invalidate = new InvalidateLineIO(nWays, indexWidth)
  // Miss queue control
  val wbInfo = Flipped(new WbFifoInfoIO(nMshrs))
  val missFifoPush = Flipped(new MshrPushIO(nCores, nMshrs, nWays, reqIdWidth, tagWidth, indexWidth, blockOffWidth, subBlockWidth))
  val isMissPushCrit = Output(Bool())
  // Output to the next stage
  val read = Flipped(new ReadIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffWidth, blockWidth, subBlockWidth))
  val halfMissCapacity = Output(Bool())
  val evictionLineBusy = Output(Bool())
}

/**
 * Rep stage of the cache pipeline
 */
class Rep(nCores: Int, nSets: Int, nWays: Int, nMshrs: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int, subBlockWidth: Int, useInvalidate: Boolean = true) extends Module() {
  val io = IO(new RepTopIO(nCores, nSets, nWays, nMshrs, reqIdWidth, tagWidth, indexWidth, blockWidth, subBlockWidth))

  def updateTagsAndDirtyBits(inDirty: Vec[Bool], inTags: Vec[UInt], currIdx: UInt): NewTagsAndDirtyBits = {
    val newDirtyBits = VecInit(Seq.fill(nWays)(false.B))
    val newTags = VecInit(Seq.fill(nWays)(0.U(tagWidth.W)))
    newDirtyBits := inDirty
    newTags := inTags

    when (io.setLineValid.refill && io.setLineValid.index === currIdx) {
      newTags(io.setLineValid.way) := io.setLineValid.tag
    }

    when(io.dirtyCtrl.set && io.dirtyCtrl.wIndex === currIdx) {
      newDirtyBits(io.dirtyCtrl.wWay) := true.B
    }.elsewhen(io.dirtyCtrl.unset && io.dirtyCtrl.wIndex === currIdx) {
      newDirtyBits(io.dirtyCtrl.wWay) := false.B
    }

    NewTagsAndDirtyBits(newDirtyBits, newTags)
  }

  def checkIfHitSetLineValid(isHitCurr: Bool, currHitWay: UInt, currIdx: UInt, currTag: UInt, invalidate: Bool, invalidateIdx: UInt, invalidateWay: UInt, useInvalidate: Boolean = true): IsHitResult = {
    val hit = WireDefault(false.B)
    val hitWay = WireDefault(0.U(log2Up(nWays).W))

    if (useInvalidate) {
      when(invalidate && invalidateIdx === currIdx && invalidateWay === currHitWay) {
        hit := false.B
      }.elsewhen(io.setLineValid.refill && io.setLineValid.index === currIdx && io.setLineValid.tag === currTag) {
        hit := true.B
        hitWay := io.setLineValid.way
      }.otherwise {
        hit := isHitCurr
        hitWay := currHitWay
      }
    } else {
      when(io.setLineValid.refill && io.setLineValid.index === currIdx && io.setLineValid.tag === currTag) {
        hit := true.B
        hitWay := io.setLineValid.way
      }.otherwise {
        hit := isHitCurr
        hitWay := currHitWay
      }
    }

    IsHitResult(hit, hitWay)
  }

  val invalidate = WireDefault(false.B)
  val invalidateIdx = WireDefault(0.U(log2Up(nSets).W))
  val invalidateWay = WireDefault(0.U(log2Up(nWays).W))

  // ---------------- Compute Replace Way ----------------
  // This value is not delayed by one CC (like other io input values that come from a pipeline reg),
  // since it is used to read the PLRU bits from the memory
  io.repPolCtrl.setIdx := io.rep.repPolReadIndex
  io.repPolCtrl.stall := io.stall
  io.repPolCtrl.coreId := io.rep.coreId
  io.repPolCtrl.valid := io.rep.reqValid

  val tagDirtyUpdate1 = updateTagsAndDirtyBits(io.rep.dirtyBits, io.rep.setTags, io.rep.index)
  val hitCheck1 = checkIfHitSetLineValid(io.rep.reqHit, io.rep.reqHitWay, io.rep.index, io.rep.tag, invalidate, invalidateIdx, invalidateWay)

  val coreIdReg1 = PipelineReg(io.rep.coreId, 0.U, !io.stall && !io.repPolCtrl.insertBubble)
  val reqValidReg1 = PipelineReg(io.rep.reqValid, false.B, !io.stall && !io.repPolCtrl.insertBubble)
  val isReqHitReg1 = PipelineReg(hitCheck1.hit, false.B, !io.stall && !io.repPolCtrl.insertBubble)
  val reqHitWayReg1 = PipelineReg(hitCheck1.hitIdx, 0.U, !io.stall && !io.repPolCtrl.insertBubble)
  val reqIdReg1 = PipelineReg(io.rep.reqId, 0.U, !io.stall && !io.repPolCtrl.insertBubble)
  val reqRwReg1 = PipelineReg(io.rep.reqRw, false.B, !io.stall && !io.repPolCtrl.insertBubble)
  val wDataReg1 = PipelineReg(io.rep.wData, 0.U, !io.stall && !io.repPolCtrl.insertBubble)
  val byteEnReg1 = PipelineReg(io.rep.byteEn, 0.U, !io.stall && !io.repPolCtrl.insertBubble)
  val dirtyBitsReg1 = PipelineReg(tagDirtyUpdate1.dirty, VecInit(Seq.fill(nWays)(false.B)), !io.stall && !io.repPolCtrl.insertBubble)
  val setTagsReg1 = PipelineReg(tagDirtyUpdate1.tags, VecInit(Seq.fill(nWays)(0.U(tagWidth.W))), !io.stall && !io.repPolCtrl.insertBubble)
  val blockOffsetReg1 = PipelineReg(io.rep.blockOffset, 0.U, !io.stall && !io.repPolCtrl.insertBubble)
  val indexReg1 = PipelineReg(io.rep.index, 0.U, !io.stall && !io.repPolCtrl.insertBubble)
  val tagReg1 = PipelineReg(io.rep.tag, 0.U, !io.stall && !io.repPolCtrl.insertBubble)

  // ---------------- Replacement policy filter ----------------
  val tagDirtyUpdate2 = updateTagsAndDirtyBits(dirtyBitsReg1, setTagsReg1, indexReg1)
  // useInvalidate parameter is only high when using contention replacement policy
  val hitCheck2 = checkIfHitSetLineValid(isReqHitReg1, reqHitWayReg1, indexReg1, tagReg1, invalidate, invalidateIdx, invalidateWay, useInvalidate)

  io.repPolInfo.isHit := hitCheck2.hit
  io.repPolInfo.hitWay := hitCheck2.hitIdx

  val coreIdReg2 = PipelineReg(coreIdReg1, 0.U, !io.stall, io.repPolCtrl.insertBubble)
  val reqValidReg2 = PipelineReg(reqValidReg1, false.B, !io.stall, io.repPolCtrl.insertBubble)
  val isReqHitReg2 = PipelineReg(hitCheck2.hit, false.B, !io.stall, io.repPolCtrl.insertBubble)
  val reqHitWayReg2 = PipelineReg(hitCheck2.hitIdx, 0.U, !io.stall, io.repPolCtrl.insertBubble)
  val reqIdReg2 = PipelineReg(reqIdReg1, 0.U, !io.stall, io.repPolCtrl.insertBubble)
  val reqRwReg2 = PipelineReg(reqRwReg1, false.B, !io.stall, io.repPolCtrl.insertBubble)
  val wDataReg2 = PipelineReg(wDataReg1, 0.U, !io.stall, io.repPolCtrl.insertBubble)
  val byteEnReg2 = PipelineReg(byteEnReg1, 0.U, !io.stall, io.repPolCtrl.insertBubble)
  val dirtyBitsReg2 = PipelineReg(tagDirtyUpdate2.dirty, VecInit(Seq.fill(nWays)(false.B)), !io.stall, io.repPolCtrl.insertBubble)
  val setTagsReg2 = PipelineReg(tagDirtyUpdate2.tags, VecInit(Seq.fill(nWays)(0.U(tagWidth.W))), !io.stall, io.repPolCtrl.insertBubble)
  val blockOffsetReg2 = PipelineReg(blockOffsetReg1, 0.U, !io.stall, io.repPolCtrl.insertBubble)
  val indexReg2 = PipelineReg(indexReg1, 0.U, !io.stall, io.repPolCtrl.insertBubble)
  val tagReg2 = PipelineReg(tagReg1, 0.U, !io.stall, io.repPolCtrl.insertBubble)

  // TODO: Think if we want to bring back the updating of hit in here?
  //  we can check if it became a hit, if so not update replacement policy
  // ---------------- Update Replacement policy ----------------
  val repWay = io.repPolCtrl.replaceWay
  val repWayValid = io.repPolCtrl.isValid

  // In case the line is set valid, we must avoid having two lines with the same tag
  val hitWithoutUpdate = WireDefault(false.B)
  val hitWithoutUpdateWay = WireDefault(0.U(log2Up(nWays).W))
  when(!isReqHitReg2 && io.setLineValid.refill && io.setLineValid.index === indexReg2 && io.setLineValid.tag === tagReg2) {
    hitWithoutUpdate := true.B
    hitWithoutUpdateWay := io.setLineValid.way
  }

  val mshrCheckLogic = Module(new MshrInfoCheck(tagWidth, nWays, nSets, nMshrs))
  mshrCheckLogic.io.idx := indexReg2
  mshrCheckLogic.io.tag := tagReg2
  mshrCheckLogic.io.reqHit := isReqHitReg2 || hitWithoutUpdate
  mshrCheckLogic.io.repWay := repWay
  mshrCheckLogic.io.reqValid := reqValidReg2
  mshrCheckLogic.io.nonCritQIdxs := io.missNonCritInfo.currentIndexes
  mshrCheckLogic.io.critQIdxs := io.missCritInfo.currentIndexes
  mshrCheckLogic.io.nonCritQTags := io.missNonCritInfo.currentTags
  mshrCheckLogic.io.critQTags := io.missCritInfo.currentTags
  mshrCheckLogic.io.nonCritQValid := io.missNonCritInfo.validMshrs
  mshrCheckLogic.io.critQValid := io.missCritInfo.validMshrs
  mshrCheckLogic.io.nonCritQRepWays := io.missNonCritInfo.replacementWays
  mshrCheckLogic.io.critQRepWays := io.missCritInfo.replacementWays
  mshrCheckLogic.io.nonCritHalfMissFull := io.missNonCritInfo.fullCmds
  mshrCheckLogic.io.critHalfMissFull := io.missCritInfo.fullCmds
  mshrCheckLogic.io.nonCritQCritCores := io.missNonCritInfo.critMshrs
  mshrCheckLogic.io.wbQValid := io.wbInfo.wbQueueValidReqs
  mshrCheckLogic.io.wbQCritCores := io.wbInfo.wbQueueCritReqs

  val tagDirtyUpdate3 = updateTagsAndDirtyBits(dirtyBitsReg2, setTagsReg2, indexReg2)
  val isRepLineDirty = tagDirtyUpdate3.dirty(repWay)
  val dirtyTag = tagDirtyUpdate3.tags(repWay)

  val reject = reqValidReg2 && !isReqHitReg2 && !repWayValid && !hitWithoutUpdate
  val evict = reqValidReg2 && !isReqHitReg2 && !hitWithoutUpdate && repWayValid && !mshrCheckLogic.io.halfMiss && !reject
  val update = reqValidReg2 && (isReqHitReg2 || repWayValid) && !hitWithoutUpdate && !mshrCheckLogic.io.halfMiss
  val halfMissPush = reqValidReg2 && mshrCheckLogic.io.halfMiss && !hitWithoutUpdate
  val updateByteEn = mshrCheckLogic.io.halfMiss && reqRwReg2 && reqValidReg2 && !hitWithoutUpdate

  // --------------- Replacement policy connections ---------------
  // Update the replacement policy even on a miss, since this miss later turns into a hit anyway.
  io.repPolCtrl.update := update && !io.stall
  io.repPolCtrl.evict := evict && !io.stall

  io.repPolInfo.nonCritMisses := mshrCheckLogic.io.nonCritMisses
  io.repPolInfo.nonCritWbs := mshrCheckLogic.io.nonCritWbs

  // --------------- Miss fifo connections ---------------
  io.isMissPushCrit := mshrCheckLogic.io.halfMissCrit || (evict && io.repPolInfo.updateCoreReachedLimit)

  io.missFifoPush.pushReq := evict && !io.stall
  io.missFifoPush.pushReqEntry.tag := tagReg2
  io.missFifoPush.pushReqEntry.index := indexReg2
  io.missFifoPush.pushReqEntry.byteEn := Mux(reqRwReg2, byteEnReg2, 0.U)
  io.missFifoPush.pushReqEntry.replaceWay := repWay
  io.missFifoPush.pushReqEntry.incidentCoreId := coreIdReg2
  io.missFifoPush.pushReqEntry.isCrit := io.repPolInfo.updateCoreIsCrit

  io.missFifoPush.pushCmd := halfMissPush && !io.stall
  io.missFifoPush.mshrIdx := mshrCheckLogic.io.halfMissIdx
  io.missFifoPush.pushCmdEntry.reqId := reqIdReg2
  io.missFifoPush.pushCmdEntry.coreId := coreIdReg2
  io.missFifoPush.pushCmdEntry.blockOffset := blockOffsetReg2

  // Update a byte mask if it is a write request and a half miss
  io.missFifoPush.updateByteEn := updateByteEn && !io.stall
  io.missFifoPush.updateByteEnVal := byteEnReg2
  io.missFifoPush.updateByteEnCol := blockOffsetReg2
  io.missFifoPush.updateByteEnRow := mshrCheckLogic.io.halfMissIdx

  // --------------- Rejection queue connections ---------------
  io.pushReject := reject && !io.stall
  io.pushRejectEntry.coreId := coreIdReg2
  io.pushRejectEntry.reqId := reqIdReg2
  io.pushRejectEntry.addr := Cat(tagReg2, indexReg2, blockOffsetReg2, 0.U(log2Up(subBlockWidth / 8).W))
  io.pushRejectEntry.rw := reqRwReg2
  io.pushRejectEntry.byteEn := byteEnReg2
  io.pushRejectEntry.wData := wDataReg2

  // --------------- Outputs ---------------
  val byteShift = Cat(blockOffsetReg2, 0.U(log2Up(subBlockWidth / 8).W))
  val blockByteMask = (byteEnReg2 << byteShift).asUInt
  val readRepWay = Mux(mshrCheckLogic.io.halfMiss, mshrCheckLogic.io.halfMissRepWay, repWay)

  io.read.coreId := coreIdReg2
  io.read.wb := evict && isRepLineDirty
  io.read.isRepWayCrit := io.repPolInfo.isReplacementWayCrit
  io.read.repWayAtLimit := io.repPolInfo.isReplacementWayAtLimit
  io.read.reqValid := reqValidReg2
  io.read.reqId := reqIdReg2
  io.read.reqRw := reqRwReg2
  io.read.wData := wDataReg2
  io.read.byteEn := blockByteMask((blockWidth / 8) -1, 0)
  io.read.repValid := repWayValid
  io.read.repWay := readRepWay
  io.read.isHit := isReqHitReg2 || hitWithoutUpdate
  io.read.hitWay := Mux(hitWithoutUpdate, hitWithoutUpdateWay, reqHitWayReg2)
  io.read.isRepDirty := isRepLineDirty
  io.read.dirtyTag := dirtyTag
  io.read.blockOffset := blockOffsetReg2
  io.read.index := indexReg2
  io.read.tag := tagReg2

  // Cache line invalidation logic
  invalidate := evict
  invalidateIdx := indexReg2
  invalidateWay := repWay
  io.invalidate.invalidate := invalidate
  io.invalidate.index := invalidateIdx
  io.invalidate.way := invalidateWay

  // --------------- Stalls due to hazards ---------------
  io.evictionLineBusy := mshrCheckLogic.io.reservedRepWay
  io.halfMissCapacity := mshrCheckLogic.io.halfMissCapacity
}
