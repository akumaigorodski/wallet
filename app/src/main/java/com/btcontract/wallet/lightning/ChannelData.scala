package com.btcontract.wallet.lightning

import com.btcontract.wallet.Utils.Bytes
import crypto.ShaChain.HashesWithLastIndex
import crypto.ShaChain
import org.bitcoinj.core.Utils.HEX
import collection.JavaConverters.asScalaBufferConverter
import org.bitcoinj.core.{Sha256Hash, TransactionOutput, Transaction, ECKey}
import com.softwaremill.quicklens._
import Tools._


trait ChannelData {
  // Third Boolean indicates whether it was saved remotely
  type ChannelSnapshot = (List[Symbol], ChannelData, Boolean)
}

// CHANNEL STATES

// We won't fund an anchor so we're waiting for it's specs from counterparty
case class WaitForAnchor(ourParams: OurChannelParams, theirParams: TheirChannelParams,
                         theirRevocationHash: proto.sha256_hash, theirNextRevocationHash: proto.sha256_hash) extends ChannelData

// We will fund and anchor and we're waiting for a first commit sig from counterparty
case class WaitForCommitSig(ourParams: OurChannelParams, theirParams: TheirChannelParams, anchor: Anchor,
                            theirCommit: TheirCommit, theirNextRevocationHash: proto.sha256_hash) extends ChannelData

// We're waiting for local confirmations + counterparty's acknoledgment before we can move on
case class WaitForConfirms(commits: Commitments, theyConfirmed: Boolean, depthOk: Boolean) extends ChannelData

// The channel is closing, tx may be re-broadcasted
case class WaitForUniclose(commit: OurCommit) extends ChannelData

// STATIC CHANNEL PARAMETERS

case class Anchor(tx: Transaction, idx: Int) {
  val hash = Sha256Hash twiceOf tx.unsafeBitcoinSerialize
  val id = HEX encode hash.getReversedBytes
  val value = output.getValue.value
  def output = tx getOutput idx
}

case class TheirChannelParams(delay: Int, commitPubKey: ECKey, finalPubKey: ECKey, minDepth: Int, initialFeeRate: Long)
case class OurChannelParams(delay: Int, anchorAmount: Option[Long], commitPrivKey: ECKey, finalPrivKey: ECKey, minDepth: Int,
                            initialFeeRate: Long, shaSeed: Bytes) extends ChannelData {

  def finalPubKey = ECKey fromPublicOnly finalPrivKey.getPubKey
  def commitPubKey = ECKey fromPublicOnly commitPrivKey.getPubKey

  def toOpenProto(anchorOfferProto: proto.open_channel.anchor_offer) = new proto.open_channel(blocks2Locktime(delay),
    Tools bytes2Sha ShaChain.revIndexFromSeed(shaSeed, 0), Tools bytes2Sha ShaChain.revIndexFromSeed(shaSeed, 1),
    ecKey2Proto(commitPubKey), ecKey2Proto(finalPubKey), anchorOfferProto, minDepth, initialFeeRate)
}

// CURRENT CHANNEL STATE

// Changes pile up until we update our commitment
case class TheirChanges(proposed: PktVec, acked: PktVec)
case class OurChanges(proposed: PktVec, signed: PktVec, acked: PktVec) {
  def fulfills: PartialFunction[proto.pkt, proto.rval] = { case pkt if has(pkt.update_fulfill_htlc) => pkt.update_fulfill_htlc.r }
  def hashRValMap = (proposed ++ signed ++ acked).collect(fulfills).map(rValue => r2HashProto(rValue) -> rValue).toMap
}

// We need to track our commit and our view of their commit
// In case if they spend their commit we can use ours to claim the funds
case class OurCommit(index: Long, spec: CommitmentSpec, publishableTx: Transaction)
case class TheirCommit(index: Long, spec: CommitmentSpec, revocationHash: proto.sha256_hash) {
  def preimageMatch(revPreImage: proto.sha256_hash) = pre2HashProto(revPreImage) == revocationHash
}

// Incoming: they are paying, outgoing: we are paying
case class Htlc(add: proto.update_add_htlc, incoming: Boolean)
case class CommitmentSpec(htlcs: Set[Htlc], feeRate: Long, initAmountUsMsat: Long,
                          initAmountThemMsat: Long, amountUsMsat: Long, amountThemMsat: Long) { me =>

  def addIncomingHtlc(their: proto.pkt) =
    copy(htlcs = htlcs + Htlc(add = their.update_add_htlc, incoming = true),
      amountThemMsat = amountThemMsat - their.update_add_htlc.amount_msat)

  def addOutgoingHtlc(our: proto.pkt) =
    copy(htlcs = htlcs + Htlc(add = our.update_add_htlc, incoming = false),
      amountUsMsat = amountUsMsat - our.update_add_htlc.amount_msat)

  def fulfillHtlc(ufh: proto.update_fulfill_htlc) =
    htlcs.find(htlc => ufh.id == htlc.add.id && r2HashProto(ufh.r) == htlc.add.r_hash) match {
      case Some(htlc) if htlc.incoming => copy(amountUsMsat = amountUsMsat + htlc.add.amount_msat, htlcs = htlcs - htlc)
      case Some(htlc) => copy(amountThemMsat = amountThemMsat + htlc.add.amount_msat, htlcs = htlcs - htlc)
      case _ => me
    }

  def failHtlc(fail: proto.update_fail_htlc) = htlcs.find(fail.id == _.add.id) match {
    case Some(htlc) if htlc.incoming => copy(amountThemMsat = amountThemMsat + htlc.add.amount_msat, htlcs = htlcs - htlc)
    case Some(htlc) => copy(amountUsMsat = amountUsMsat + htlc.add.amount_msat, htlcs = htlcs - htlc)
    case _ => me
  }

  def reduce(ourChanges: PktVec, theirChanges: PktVec) = {
    val spec1 = (me /: ourChanges) { case (s, pkt) if has(pkt.update_add_htlc) => s addOutgoingHtlc pkt case (s, _) => s }
    val spec2 = (spec1 /: theirChanges) { case (s, pkt) if has(pkt.update_add_htlc) => s addIncomingHtlc pkt case (s, _) => s }

    val spec3 = (ourChanges ++ theirChanges).foldLeft(spec2) {
      case (s, pkt) if has(pkt.update_fulfill_htlc) => s fulfillHtlc pkt.update_fulfill_htlc
      case (s, pkt) if has(pkt.update_fail_htlc) => s failHtlc pkt.update_fail_htlc
      case (s, _) => s
    }

    spec3
  }
}

case class Commitments(ourParams: OurChannelParams, theirParams: TheirChannelParams, ourChanges: OurChanges, theirChanges: TheirChanges,
                       ourCommit: OurCommit, theirCommit: TheirCommit, theirNextCommitInfo: Either[TheirCommit, proto.sha256_hash],
                       anchorOutput: TransactionOutput, anchorId: String, theirPreimages: HashesWithLastIndex = (None, Map.empty),
                       start: Long = System.currentTimeMillis, shutdown: Option[Long] = None) extends ChannelData { me =>

  def weHaveChanges = theirChanges.acked.nonEmpty | ourChanges.proposed.nonEmpty
  def theyHaveChanges = ourChanges.acked.nonEmpty | theirChanges.proposed.nonEmpty
  def addOurProposal(proposal: proto.pkt) = me.modify(_.ourChanges.proposed).using(_ :+ proposal)
  def addTheirProposal(proposal: proto.pkt) = me.modify(_.theirChanges.proposed).using(_ :+ proposal)
  def finalFee = anchorOutput.getValue.subtract(ourCommit.publishableTx.getOutputSum) div 4 multiply 2

  // OUR AND THEIR COMMIT TXS

  def makeOurTxTemplate(ourRevHash: Bytes) =
    Scripts.makeCommitTxTemplate(ourFinalKey = ourParams.finalPubKey,
      theirParams.finalPubKey, ourParams.delay, ourRevHash, ourCommit.spec)

  // spec may come from theirCommit or theirNextCommitInfo
  def makeTheirTxTemplate(theirRevHash: Bytes, spec: CommitmentSpec) =
    Scripts.makeCommitTxTemplate(theirParams.finalPubKey, ourParams.finalPubKey,
      theirParams.delay, theirRevHash, spec)

  // FINAL TX AND FEE NEGOTIATIONS

  def finalTx(fee: Long) = Scripts.makeFinalTx(ourCommit.publishableTx.getInputs.asScala,
    ourParams.finalPubKey, theirParams.finalPubKey, ourCommit.spec.amountUsMsat,
    theirCommit.spec.amountUsMsat, fee)

  def makeNewFeeFinalSig(them: Long, us: Long) = {
    val newFee = (them + us) / 4 * 2 match { case fee if fee == us => fee + 2 case fee => fee }
    finalTx(newFee) match { case (transaction, closeSigProto) => closeSigProto }
  }

  def checkCloseSig(closeProto: proto.close_signature) = finalTx(closeProto.close_fee) match { case (tx, ourSig) =>
    val signedFinalTx = Scripts.addTheirSigAndSignTx(ourParams, theirParams, tx, anchorOutput.getValue.value, closeProto.sig)
    (Scripts.brokenTxCheck(signedFinalTx, anchorOutput).isSuccess, signedFinalTx, ourSig)
  }
}