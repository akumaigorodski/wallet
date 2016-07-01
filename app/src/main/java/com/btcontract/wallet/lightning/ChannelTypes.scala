package com.btcontract.wallet.lightning

import ChannelTypes._
import org.bitcoinj.core._
import com.softwaremill.quicklens._
import Tools.{rProto2Proto, toPkt, sha2Bytes, bytes2Sha}
import crypto.ShaChain.HashesWithLastIndex
import com.btcontract.wallet.Utils.Bytes


object ChannelTypes {
  type PktVec = Vector[proto.pkt]
  def makeOurTx(ourParams: OurChannelParams, theirParams: TheirChannelParams,
                inputs: java.util.List[TransactionInput], ourRevocationHash: Bytes, spec: CommitmentSpec) =
                Scripts.makeCommitTx(inputs, ourParams.finalPrivKey.getPubKey, theirParams.finalPubKey.getPubKey,
                  ourParams.delay, ourRevocationHash, spec)

  def makeTheirTx(ourParams: OurChannelParams, theirParams: TheirChannelParams,
                  inputs: java.util.List[TransactionInput], theirRevocationHash: Bytes, spec: CommitmentSpec) =
                  Scripts.makeCommitTx(inputs, theirParams.finalPubKey.getPubKey, ourParams.finalPrivKey.getPubKey,
                    theirParams.delay, theirRevocationHash, spec)
}

// If anchorAmount is None we don't fund a channel
case class OurChannelParams(delay: Int, commitPrivKey: ECKey, finalPrivKey: ECKey, minDepth: Int,
                            initialFeeRate: Long, anchorAmount: Option[Long], shaSeed: Bytes)

case class TheirChannelParams(delay: Int, commitPubKey: ECKey, finalPubKey: ECKey,
                              minDepth: Int, initialFeeRate: Long)

// Direction true means IN, false means OUT
case class Htlc(direction: Boolean, id: Long, amountMsat: Int, rHash: Bytes,
                nextNodeIds: Seq[String], previousChannelId: Option[Bytes], expiry: Int)

case class CommitmentSpec(htlcs: Set[Htlc], feeRate: Long, initAmountUsMsat: Long,
                          initAmountThemMsat: Long, amountUsMsat: Long,
                          amountThemMsat: Long) { me =>

  def addHtlc(direction: Boolean, u: proto.update_add_htlc) = {
    val htlc = Htlc(direction, u.id, u.amount_msat, sha2Bytes(u.r_hash), Seq.empty, None, u.expiry.blocks)
    if (direction) copy(amountThemMsat = amountThemMsat - htlc.amountMsat, htlcs = htlcs + htlc)
    else copy(amountUsMsat = amountUsMsat - htlc.amountMsat, htlcs = htlcs + htlc)
  }

  // direction = false means we are sending an update_fulfill_htlc
  // message which means that we are fulfilling an HTLC they've sent
  def fulfillHtlc(direction: Boolean, u: proto.update_fulfill_htlc) =
    htlcs collectFirst { case htlc if u.id == htlc.id && rProto2Proto(u.r) == bytes2Sha(htlc.rHash) => htlc } match {
      case Some(htlc) if htlc.direction => copy(amountThemMsat = amountThemMsat + htlc.amountMsat, htlcs = htlcs - htlc)
      case Some(htlc) => copy(amountUsMsat = amountUsMsat + htlc.amountMsat, htlcs = htlcs - htlc)
      case _ => me
    }

  // direction = false means we are sending an update_fail_htlc
  // message which means that we are failing an HTLC they've sent
  def failHtlc(direction: Boolean, fail: proto.update_fail_htlc) = htlcs.find(_.id == fail.id) match {
    case Some(htlc) if htlc.direction => copy(amountUsMsat = amountUsMsat + htlc.amountMsat, htlcs = htlcs - htlc)
    case Some(htlc) => copy(amountThemMsat = amountThemMsat + htlc.amountMsat, htlcs = htlcs - htlc)
    case _ => me
  }

  def reduce(ourChanges: PktVec, theirChanges: PktVec) = {
    val spec = copy(htlcs = Set.empty, amountUsMsat = initAmountUsMsat, amountThemMsat = initAmountThemMsat)
    val spec1 = (spec /: ourChanges) { case (s, pkt) if pkt.update_add_htlc != null => s.addHtlc(direction = false, pkt.update_add_htlc) case (s, _) => s }
    val spec2 = (spec1 /: theirChanges) { case (s, pkt) if pkt.update_add_htlc != null => s.addHtlc(direction = true, pkt.update_add_htlc) case (s, _) => s }

    val spec3 = (spec2 /: ourChanges) {
      case (s, pkt) if pkt.update_fulfill_htlc != null => s.fulfillHtlc(direction = false, pkt.update_fulfill_htlc)
      case (s, pkt) if pkt.update_fail_htlc != null => s.failHtlc(direction = false, pkt.update_fail_htlc)
      case (s, _) => s
    }

    val spec4 = (spec3 /: theirChanges) {
      case (s, pkt) if pkt.update_fulfill_htlc != null => s.fulfillHtlc(direction = true, pkt.update_fulfill_htlc)
      case (s, pkt) if pkt.update_fail_htlc != null => s.failHtlc(direction = true, pkt.update_fail_htlc)
      case (s, _) => s
    }

    spec4
  }
}

case class OurCommit(spec: CommitmentSpec, publishableTx: Transaction)
case class TheirCommit(spec: CommitmentSpec, theirRevocationHash: Bytes)
case class OurChanges(proposed: PktVec, signed: PktVec, acked: PktVec)
case class TheirChanges(proposed: PktVec, acked: PktVec)

case class Commitments(ourParams: OurChannelParams, theirParams: TheirChannelParams, ourChanges: OurChanges, theirChanges: TheirChanges,
                       ourCommit: OurCommit, theirCommit: TheirCommit, theirNextCommitInfo: Either[TheirCommit, Bytes],
                       theirPreimages: HashesWithLastIndex, anchorOutput: TransactionOutput) { me =>

  def anchorId = {
    val inputs = ourCommit.publishableTx.getInputs
    require(inputs.size == 1, "CommitTx should have one input")
    inputs.get(0).getOutpoint.getHash
  }

  def hasNoPendingHtlcs = ourCommit.spec.htlcs.isEmpty & theirCommit.spec.htlcs.isEmpty
  def addOurProposal(proposal: proto.pkt) = me.modify(_.ourChanges.proposed).using(_ :+ proposal)
  def addTheirProposal(proposal: proto.pkt) = me.modify(_.theirChanges.proposed).using(_ :+ proposal)
  def id(id: java.lang.Long)(pkt: proto.pkt) = pkt.update_add_htlc != null && pkt.update_add_htlc.id == id

  def sendFulfill(fulfill: proto.update_fulfill_htlc) =
    // They have previously sent this HTLC to me, now I have an r-value and propose to fulfill it
    theirChanges.acked collectFirst { case pkt if id(fulfill.id)(pkt) => pkt.update_add_htlc } match {
      case Some(foundHtlc) if rProto2Proto(fulfill.r) == foundHtlc.r_hash => me addOurProposal toPkt(fulfill)
      case Some(foundHtlc) => throw new Exception("Invalid htlc preimage for id" + fulfill.id)
      case None => throw new Exception("sendFulfill")
    }

  def receiveFulfill(fulfill: proto.update_fulfill_htlc) =
    // I have previously sent this HTLC to them, now I receive a fulfill with an r-value
    ourChanges.acked collectFirst { case pkt if id(fulfill.id)(pkt) => pkt.update_add_htlc } match {
      case Some(foundHtlc) if rProto2Proto(fulfill.r) == foundHtlc.r_hash => me addTheirProposal toPkt(fulfill)
      case Some(foundHtlc) => throw new Exception("Invalid htlc preimage for id" + fulfill.id)
      case None => throw new Exception("receiveFulfill")
    }

  // I fail an acked HTLC they've previously sent to me
  def sendFail(fail: proto.update_fail_htlc) = theirChanges.acked exists id(fail.id) match {
    case true => me addOurProposal toPkt(some = fail) case false => throw new Exception("sendFail")
  }

  // I got their failure for an acked HTLC I've previously sent
  def receiveFail(fail: proto.update_fail_htlc) = ourChanges.acked exists id(fail.id) match {
    case true => me addTheirProposal toPkt(some = fail) case false => throw new Exception("receiveFail")
  }

  def sendCommit = theirNextCommitInfo match {
    // Attempting to sign twice waiting for the first revocation message
    case Left(theirNextCommit) => throw new Exception("sendCommit")

    case Right(theirNextRevocationHash) =>
      // Sign all our proposals + their acked proposals
      val spec1 = theirCommit.spec.reduce(theirChanges.acked, ourChanges.acked ++ ourChanges.signed ++ ourChanges.proposed)
      val theirTx = makeTheirTx(ourParams, theirParams, ourCommit.publishableTx.getInputs, theirNextRevocationHash, spec1)

      // Their commitment now includes all our changes + their acked changes
      val ourChanges1 = ourChanges.copy(proposed = Vector.empty, signed = ourChanges.signed ++ ourChanges.proposed)
      val commitments1 = copy(theirNextCommitInfo = Left apply TheirCommit(spec1, theirNextRevocationHash), ourChanges = ourChanges1)
      (new proto.update_commit.Builder sig Scripts.signTx(ourParams, theirParams, anchorOutput.getValue.value, theirTx), commitments1)
  }
}