package com.btcontract.wallet.lightning

import ChannelTypes._
import com.softwaremill.quicklens._
import org.bitcoinj.core.{TransactionOutput, Transaction, ECKey}
import crypto.ShaChain.HashesWithLastIndex
import com.btcontract.wallet.Utils.Bytes


object ChannelTypes {
  type PktSeq = Seq[proto.pkt]
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
                          initAmountThemMsat: Long, amountUsMsat: Long, amountThemMsat: Long)

case class OurCommit(index: Long, spec: CommitmentSpec, publishableTx: Transaction)
case class TheirCommit(index: Long, spec: CommitmentSpec, theirRevocationHash: Bytes)
case class OurChanges(proposed: PktSeq, signed: PktSeq, acked: PktSeq)
case class TheirChanges(proposed: PktSeq, acked: PktSeq)

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
}