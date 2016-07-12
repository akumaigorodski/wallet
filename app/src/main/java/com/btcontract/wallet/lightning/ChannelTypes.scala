package com.btcontract.wallet.lightning

import Tools._
import ChannelTypes._
import org.bitcoinj.core._
import com.softwaremill.quicklens._
import crypto.ShaChain.HashesWithLastIndex
import com.btcontract.wallet.Utils.Bytes
import crypto.ShaChain


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

trait ChannelData

// If anchorAmount is None we don't fund a channel
case class OurChannelParams(delay: Int, commitPrivKey: ECKey, finalPrivKey: ECKey,
                            minDepth: Int, initialFeeRate: Long, anchorAmount: Option[Long],
                            shaSeed: Bytes) extends ChannelData

case class TheirChannelParams(delay: Int, commitPubKey: ECKey, finalPubKey: ECKey,
                              minDepth: Int, initialFeeRate: Long)

case class Htlc(incoming: Boolean, id: Long, amountMsat: Int, rHash: Bytes,
                nextNodeIds: Seq[String], previousChannelId: Option[Bytes], expiry: Int)

case class CommitmentSpec(htlcs: Set[Htlc], feeRate: Long,
                          initAmountUsMsat: Long, initAmountThemMsat: Long,
                          amountUsMsat: Long, amountThemMsat: Long) { me =>

  def addHtlc(direction: Boolean, u: proto.update_add_htlc) = {
    val htlc = Htlc(direction, u.id, u.amount_msat, sha2Bytes(u.r_hash), Seq.empty, None, u.expiry.blocks)
    if (direction) copy(amountThemMsat = amountThemMsat - htlc.amountMsat, htlcs = htlcs + htlc)
    else copy(amountUsMsat = amountUsMsat - htlc.amountMsat, htlcs = htlcs + htlc)
  }

  // direction = false means we are sending an update_fulfill_htlc
  // message which means that we are fulfilling an HTLC they've sent
  def fulfillHtlc(direction: Boolean, u: proto.update_fulfill_htlc) =
    htlcs collectFirst { case htlc if u.id == htlc.id && r2HashProto(u.r) == bytes2Sha(htlc.rHash) => htlc } match {
      case Some(htlc) if htlc.incoming => copy(amountThemMsat = amountThemMsat + htlc.amountMsat, htlcs = htlcs - htlc)
      case Some(htlc) => copy(amountUsMsat = amountUsMsat + htlc.amountMsat, htlcs = htlcs - htlc)
      case _ => me
    }

  // direction = false means we are sending an update_fail_htlc
  // message which means that we are failing an HTLC they've sent
  def failHtlc(direction: Boolean, fail: proto.update_fail_htlc) = htlcs.find(_.id == fail.id) match {
    case Some(htlc) if htlc.incoming => copy(amountUsMsat = amountUsMsat + htlc.amountMsat, htlcs = htlcs - htlc)
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

case class AnchorTxData(output: TransactionOutput, txId: String, address: String)
case class OurCommit(index: Long, spec: CommitmentSpec, publishableTx: Transaction)
case class TheirCommit(index: Long, spec: CommitmentSpec, theirRevocationHash: Bytes)
case class OurChanges(proposed: PktVec, signed: PktVec, acked: PktVec)
case class TheirChanges(proposed: PktVec, acked: PktVec)

// Non empty ourClearing means I've sent a proposition to close a channel but do not have an answer yet
case class Commitments(ourClearing: Option[proto.close_clearing], ourParams: OurChannelParams, theirParams: TheirChannelParams,
                       ourChanges: OurChanges, theirChanges: TheirChanges, ourCommit: OurCommit, theirCommit: TheirCommit,
                       theirNextCommitInfo: Either[TheirCommit, Bytes], theirPreimages: HashesWithLastIndex,
                       anchorData: AnchorTxData) extends ChannelData { me =>

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
      case Some(foundHtlc) if r2HashProto(fulfill.r) == foundHtlc.r_hash => me addOurProposal toPkt(fulfill)
      case Some(foundHtlc) => throw new Exception("Invalid htlc preimage for id" + fulfill.id)
      case None => throw new RuntimeException("sendFulfill")
    }

  def receiveFulfill(fulfill: proto.update_fulfill_htlc) =
    // I have previously sent this HTLC to them, now I receive a fulfill with an r-value
    ourChanges.acked collectFirst { case pkt if id(fulfill.id)(pkt) => pkt.update_add_htlc } match {
      case Some(foundHtlc) if r2HashProto(fulfill.r) == foundHtlc.r_hash => me addTheirProposal toPkt(fulfill)
      case Some(foundHtlc) => throw new Exception("Invalid htlc preimage for id" + fulfill.id)
      case None => throw new RuntimeException("receiveFulfill")
    }

  // I fail an acked HTLC they've previously sent to me
  def sendFail(fail: proto.update_fail_htlc) = theirChanges.acked exists id(fail.id) match {
    case true => me addOurProposal toPkt(fail) case false => throw new RuntimeException("sendFail")
  }

  // I got their failure for an acked HTLC I've previously sent
  def receiveFail(fail: proto.update_fail_htlc) = ourChanges.acked exists id(fail.id) match {
    case true => me addTheirProposal toPkt(fail) case false => throw new RuntimeException("receiveFail")
  }

  def sendCommit = theirNextCommitInfo match {
    // Attempting to sign twice waiting for the first revocation message
    case Left(theirNextCommit) => throw new RuntimeException("sendCommit")

    case Right(theirNextRevocationHash) =>
      // Sign all our proposals + their acked proposals
      val spec1 = theirCommit.spec.reduce(theirChanges.acked, ourChanges.acked ++ ourChanges.signed ++ ourChanges.proposed)
      val theirTx = makeTheirTx(ourParams, theirParams, ourCommit.publishableTx.getInputs, theirNextRevocationHash, spec1)
      val ourSigForThem = Scripts.signTx(ourParams, theirParams, anchorData.output.getValue.value, theirTx)

      // Their commitment now includes all our changes + their acked changes
      val theirNextCommitInfo1 = Left apply TheirCommit(theirCommit.index + 1, spec1, theirNextRevocationHash)
      val ourChanges1 = ourChanges.copy(proposed = Vector.empty, signed = ourChanges.signed ++ ourChanges.proposed)
      new proto.update_commit(ourSigForThem) -> copy(theirNextCommitInfo = theirNextCommitInfo1, ourChanges = ourChanges1)
  }

  def receiveCommit(commit: proto.update_commit) = {
    // First we must check if we can spend a commit using their signature
    val spec1 = ourCommit.spec.reduce(ourChanges.acked, theirChanges.acked ++ theirChanges.proposed)
    val ourNextRevocationHash = Sha256Hash hash ShaChain.revIndexFromSeed(ourParams.shaSeed, ourCommit.index + 1)
    val ourTx = makeOurTx(ourParams, theirParams, ourCommit.publishableTx.getInputs, ourNextRevocationHash, spec1)
    val ourSignedTx = Scripts.addTheirSigAndSignTx(ourParams, theirParams, anchorData.output.getValue, ourTx, commit.sig)
    Scripts.checkSigOrThrow(ourSignedTx, anchorData.output)

    // We will send our revocation preimage and our next revocation hash
    val ourRevocationPreimage = ShaChain.revIndexFromSeed(ourParams.shaSeed, ourCommit.index)
    val ourNextRevocationHash1 = Sha256Hash hash ShaChain.revIndexFromSeed(ourParams.shaSeed, ourCommit.index + 2)
    val theirChanges1 = theirChanges.copy(proposed = Vector.empty, acked = theirChanges.acked ++ theirChanges.proposed)
    val revocProto = new proto.update_revocation(Tools bytes2Sha ourRevocationPreimage, Tools bytes2Sha ourNextRevocationHash1)
    revocProto -> copy(ourCommit = OurCommit(ourCommit.index + 1, spec1, ourSignedTx), theirChanges = theirChanges1)
  }

  def receiveRevocation(revoc: proto.update_revocation) = theirNextCommitInfo match {
    // Receiving an unexpected update_revocation message while not waiting for a revocation
    case Right(theirNextRevocationHash) => throw new RuntimeException("RevocationHashNotCommit")

    case Left(theirNextCommit) =>
      // We receive a revocation because we just sent them a sig for their next commit tx
      require(preimg2HashProto(revoc.revocation_preimage) == bytes2Sha(theirCommit.theirRevocationHash), "InvalidPreimage")
      copy(theirPreimages = ShaChain.revAddHash(theirPreimages, sha2Bytes(revoc.revocation_preimage), theirCommit.index),
        ourChanges = ourChanges.copy(signed = Vector.empty, acked = ourChanges.acked ++ ourChanges.signed),
        theirNextCommitInfo = Right apply sha2Bytes(revoc.next_revocation_hash),
        theirCommit = theirNextCommit)
  }
}

// Channel states for mutual closing process
// Counterparty has agreed to close a channel but we have an unresolved HTLC's
case class ChannelClearing(commits: Commitments, ourClearing: proto.close_clearing,
                           theirClearing: proto.close_clearing) extends ChannelData

case class ChannelFeeNegotiating(commits: Commitments, ourSignature: proto.close_signature,
                                 ourClearing: proto.close_clearing, theirClearing: proto.close_clearing)
                                 extends ChannelData

case class ChannelClosing(commits: Commitments, ourSignature: Option[proto.close_signature] = None,
                          mutualClosePublished: Option[Transaction] = None, ourCommitPublished: Option[Transaction] = None,
                          theirCommitPublished: Option[Transaction] = None, revokedPublished: Seq[Transaction] = Seq.empty)
                          extends ChannelData {

  require(mutualClosePublished.isDefined | ourCommitPublished.isDefined
    | theirCommitPublished.isDefined | revokedPublished.nonEmpty,
    "At least one tx has to be published in this state")
}