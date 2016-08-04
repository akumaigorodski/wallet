package com.btcontract.wallet.lightning

import Tools._
import org.bitcoinj.core._
import com.softwaremill.quicklens._
import com.btcontract.wallet.Utils.{app, Bytes}
import collection.JavaConverters.asScalaBufferConverter
import crypto.ShaChain.HashesWithLastIndex
import crypto.ShaChain


trait ChannelData {
  type SavedState = (ChannelSnapshot, Boolean)
  type ChannelSnapshot = (List[Symbol], ChannelData)
}

// CHANNEL STATES

// We won't fund an anchor so we're waiting for it's specs from counterparty
case class WaitForAnchor(ourParams: OurChannelParams, theirParams: TheirChannelParams,
                         theirRevocationHash: Bytes, theirNextRevocationHash: Bytes) extends ChannelData

// We will fund and anchor and we're waiting for a first commit sig from counterparty
case class WaitForCommitSig(ourParams: OurChannelParams, theirParams: TheirChannelParams, anchorTx: Transaction,
                            anchorIndex: Int, theirCommit: TheirCommit, theirNextRevocationHash: Bytes) extends ChannelData

// We're waiting for local confirmations + counterparty's acknoledgment before we can move on
case class WaitForConfirms(commits: Commitments, blockHash: Option[Bytes], depthOk: Boolean) extends ChannelData

// We have agreed on fee and wait for a final transaction to reach required depth
case class WaitForClosing(commits: Commitments, finalTx: Transaction) extends ChannelData

// CHANNEL PARAMETERS

case class OurChannelParams(delay: Int, anchorAmount: Option[Long], commitPrivKey: ECKey, finalPrivKey: ECKey,
                            minDepth: Int, initialFeeRate: Long, shaSeed: Bytes) extends ChannelData {

  def finalPubKey = ECKey fromPublicOnly finalPrivKey.getPubKey
  def commitPubKey = ECKey fromPublicOnly commitPrivKey.getPubKey

  def toOpenProto(anchorIntent: proto.open_channel.anchor_offer) = new proto.open_channel(Tools blocks delay,
    Tools bytes2Sha ShaChain.revIndexFromSeed(shaSeed, 0), Tools bytes2Sha ShaChain.revIndexFromSeed(shaSeed, 1),
    bytes2ProtoPubkey(commitPrivKey.getPubKey), bytes2ProtoPubkey(finalPrivKey.getPubKey),
    anchorIntent, minDepth, initialFeeRate)
}

case class TheirChannelParams(delay: Int, commitPubKey: ECKey, finalPubKey: ECKey, minDepth: Int, initialFeeRate: Long)
case class HtlcBase(amountMsat: Int, rHash: Bytes, nextNodeIds: Seq[String], previousChannelId: Option[Bytes], expiry: Int)
case class Htlc(base: HtlcBase, incoming: Boolean, id: Long)

case class CommitmentSpec(htlcs: Set[Htlc], feeRate: Long, initAmountUsMsat: Long,
                          initAmountThemMsat: Long, amountUsMsat: Long, amountThemMsat: Long) { me =>

  def addHtlc(incoming: Boolean, u: proto.update_add_htlc) = {
    val htlc = Htlc(HtlcBase(u.amount_msat, sha2Bytes(u.r_hash), Seq.empty, None, u.expiry.blocks), incoming, u.id)
    if (incoming) copy(amountThemMsat = amountThemMsat - htlc.base.amountMsat, htlcs = htlcs + htlc)
    else copy(amountUsMsat = amountUsMsat - htlc.base.amountMsat, htlcs = htlcs + htlc)
  }

  // incoming = false means we are sending an update_fulfill_htlc
  // message which means that we are fulfilling an HTLC they've sent
  def fulfillHtlc(direction: Boolean, u: proto.update_fulfill_htlc) =
    htlcs collectFirst { case htlc if u.id == htlc.id && r2HashProto(u.r) == bytes2Sha(htlc.base.rHash) => htlc } match {
      case Some(htlc) if htlc.incoming => copy(amountThemMsat = amountThemMsat + htlc.base.amountMsat, htlcs = htlcs - htlc)
      case Some(htlc) => copy(amountUsMsat = amountUsMsat + htlc.base.amountMsat, htlcs = htlcs - htlc)
      case _ => me
    }

  // incoming = false means we are sending an update_fail_htlc
  // message which means that we are failing an HTLC they've sent
  def failHtlc(direction: Boolean, fail: proto.update_fail_htlc) =
    htlcs collectFirst { case failedHtlc if fail.id == failedHtlc.id => failedHtlc } match {
      case Some(htlc) if htlc.incoming => copy(amountUsMsat = amountUsMsat + htlc.base.amountMsat, htlcs = htlcs - htlc)
      case Some(htlc) => copy(amountThemMsat = amountThemMsat + htlc.base.amountMsat, htlcs = htlcs - htlc)
      case _ => me
    }

  def reduce(ourChanges: PktVec, theirChanges: PktVec) = {
    val spec = copy(htlcs = Set.empty, amountUsMsat = initAmountUsMsat, amountThemMsat = initAmountThemMsat)
    val spec1 = (spec /: ourChanges) { case (s, pkt) if pkt.update_add_htlc != null => s.addHtlc(incoming = false, pkt.update_add_htlc) case (s, _) => s }
    val spec2 = (spec1 /: theirChanges) { case (s, pkt) if pkt.update_add_htlc != null => s.addHtlc(incoming = true, pkt.update_add_htlc) case (s, _) => s }

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

case class TheirChanges(proposed: PktVec, acked: PktVec)
case class OurChanges(proposed: PktVec, signed: PktVec, acked: PktVec)
case class OurCommit(index: Long, spec: CommitmentSpec, publishableTx: Transaction)
case class TheirCommit(index: Long, spec: CommitmentSpec, theirRevocationHash: Bytes)

case class Commitments(ourParams: OurChannelParams, theirParams: TheirChannelParams,
                       ourChanges: OurChanges, theirChanges: TheirChanges, ourCommit: OurCommit, theirCommit: TheirCommit,
                       theirNextCommitInfo: Either[TheirCommit, Bytes], anchorOutput: TransactionOutput, anchorId: String,
                       theirPreimages: HashesWithLastIndex = (None, Map.empty), started: Long = System.currentTimeMillis,
                       clearingStarted: Option[Long] = None, htlcIndex: Long = 0L) extends ChannelData { me =>

  def anchorAddressString = app.getTo(anchorOutput).toString
  def ourSciptPubKey = Scripts.pay2wpkh(ourParams.finalPubKey).build
  def theirSciptPubKey = Scripts.pay2wpkh(theirParams.finalPubKey).build
  def addOurProposal(proposal: proto.pkt) = me.modify(_.ourChanges.proposed).using(_ :+ proposal)
  def addTheirProposal(proposal: proto.pkt) = me.modify(_.theirChanges.proposed).using(_ :+ proposal)
  def finalFee = anchorOutput.getValue subtract ourCommit.publishableTx.getOutputSum div 4 multiply 2

  def makeNewFeeFinalSig(them: Long, us: Long) = {
    val newFee = (them + us) / 4 * 2 match { case fee if fee == us => fee + 2 case fee => fee }
    makeFinalTx(newFee) match { case (transaction, closeSigProto) => closeSigProto }
  }

  def makeFinalTx(fee: Long) = Scripts.makeFinalTx(ourCommit.publishableTx.getInputs.asScala,
    ourSciptPubKey, theirSciptPubKey, ourCommit.spec.amountUsMsat, theirCommit.spec.amountUsMsat, fee)

  def checkCloseSig(closeProto: proto.close_signature) = makeFinalTx(closeProto.close_fee) match { case (tx, ourSig) =>
    val signedFinalTx = Scripts.addTheirSigAndSignTx(ourParams, theirParams, tx, anchorOutput.getValue.value, closeProto.sig)
    (Scripts.brokenTxCheck(signedFinalTx, anchorOutput).isSuccess, signedFinalTx, ourSig)
  }

  def findAddHtlcOpt(packets: PktVec, id: java.lang.Long) = packets collectFirst {
    case pkt if pkt.update_add_htlc != null && pkt.update_add_htlc.id == id => pkt.update_add_htlc
  }
}