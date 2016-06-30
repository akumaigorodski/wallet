package com.btcontract.wallet.lightning

import ChannelTypes._
import com.softwaremill.quicklens._
import org.bitcoinj.core.{TransactionOutput, Transaction, ECKey}
import crypto.ShaChain.HashesWithLastIndex
import com.btcontract.wallet.Utils.Bytes
import Tools.{rToHash, toPkt}


object ChannelTypes {
  type PktVec = Vector[proto.pkt]
}

// Extended exceptions
class UnknownHtlcId(where: Symbol, id: java.lang.Long) extends Exception

// If anchorAmount is None we don't fund a channel
case class OurChannelParams(delay: Int, commitPrivKey: ECKey, finalPrivKey: ECKey, minDepth: Int,
                            initialFeeRate: Long, anchorAmount: Option[Long], shaSeed: Bytes)

case class TheirChannelParams(delay: Int, commitPubKey: ECKey, finalPubKey: ECKey,
                              minDepth: Int, initialFeeRate: Long)

// Direction true means IN, false means OUT
case class Htlc(direction: Boolean, id: Long, amountMsat: Int, rHash: Bytes,
                nextNodeIds: Seq[String], previousChannelId: Option[Bytes], expiry: Int)

case class CommitmentSpec(htlcs: Set[Htlc], feeRate: Long, initAmountUsMsat: Long,
                          initAmountThemMsat: Long, amountUsMsat: Long, amountThemMsat: Long)

case class OurCommit(index: Long, spec: CommitmentSpec, publishableTx: Transaction)
case class TheirCommit(index: Long, spec: CommitmentSpec, theirRevocationHash: Bytes)
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
  def ok(id: java.lang.Long)(pkt: proto.pkt) = pkt.update_add_htlc != null && pkt.update_add_htlc.id == id

  def sendFulfill(fulfill: proto.update_fulfill_htlc) =
    // They have previously sent this HTLC to me, now I have an r-value and propose to fulfill it
    theirChanges.acked collectFirst { case pkt if ok(fulfill.id)(pkt) => pkt.update_add_htlc } match {
      case Some(foundHtlc) if foundHtlc.r_hash == rToHash(fulfill.r) => me addOurProposal toPkt(fulfill)
      case Some(foundHtlc) => throw new Exception("Invalid htlc preimage for id" + fulfill.id)
      case None => throw new UnknownHtlcId('sendFulfill, fulfill.id)
    }

  def receiveFulfill(fulfill: proto.update_fulfill_htlc) =
    // I have previously sent this HTLC to them, now I receive a fulfill with an r-value
    ourChanges.acked collectFirst { case pkt if ok(fulfill.id)(pkt) => pkt.update_add_htlc } match {
      case Some(foundHtlc) if foundHtlc.r_hash == rToHash(fulfill.r) => me addTheirProposal toPkt(fulfill)
      case Some(foundHtlc) => throw new Exception("Invalid htlc preimage for id" + fulfill.id)
      case None => throw new UnknownHtlcId('receiveFulfill, fulfill.id)
    }

  // I fail an acked HTLC they've previously sent to me
  def sendFail(fail: proto.update_fail_htlc) = theirChanges.acked exists ok(fail.id) match {
    case true => me addOurProposal toPkt(fail) case false => throw new UnknownHtlcId('sendFail, fail.id)
  }

  // I got their failure for an acked HTLC I've previously sent
  def receiveFail(fail: proto.update_fail_htlc) = ourChanges.acked exists ok(fail.id) match {
    case true => me addTheirProposal toPkt(fail) case false => throw new UnknownHtlcId('receiveFail, fail.id)
  }
}