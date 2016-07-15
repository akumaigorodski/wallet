package com.btcontract.wallet.lightning

import Tools._
import org.bitcoinj.core._
import com.btcontract.wallet.Utils.app
import org.bitcoinj.core.Utils.HEX
import crypto.ShaChain


abstract class Channel(state: List[Symbol], data: ChannelData)
extends StateMachine[ChannelData](state, data) { me =>
  val authHandler: AuthHandler

  def doProcess(change: Any) = (change, data, state) match {
    case (paramsWithAnchor: OurChannelParams, null, 'inactive :: rest) if paramsWithAnchor.anchorAmount.isDefined =>
      authHandler process paramsWithAnchor.toOpenProto(proto.open_channel.anchor_offer.WILL_CREATE_ANCHOR)
      become(paramsWithAnchor, 'openWaitForOpenWithAnchor)

    case (paramsNoAnchor: OurChannelParams, null, 'inactive :: rest) if paramsNoAnchor.anchorAmount.isEmpty =>
      authHandler process paramsNoAnchor.toOpenProto(proto.open_channel.anchor_offer.WONT_CREATE_ANCHOR)
      become(paramsNoAnchor, 'openWaitForOpenNoAnchor)

    // INIT

    case (pkt: proto.pkt, params: OurChannelParams, 'openWaitForOpenWithAnchor :: rest)
      if pkt.open != null && pkt.open.anch == proto.open_channel.anchor_offer.WONT_CREATE_ANCHOR =>

      val amount = params.anchorAmount.get
      val theirCommitPubKey = proto2ECKey(pkt.open.commit_key)
      val theirRevocationHash = Tools sha2Bytes pkt.open.revocation_hash
      val theirNextRevocationHash = Tools sha2Bytes pkt.open.next_revocation_hash
      val anchorTx = Scripts.makeAnchorTx(params.commitPubKey, theirCommitPubKey, amount)
      val nonReversedTxHash = bytes2Sha(Sha256Hash hashTwice anchorTx.unsafeBitcoinSerialize)

      authHandler process new proto.open_anchor(nonReversedTxHash, 0, amount)
      become(WaitForCommitSig(params, TheirChannelParams(pkt.open.delay.blocks, theirCommitPubKey,
        proto2ECKey(pkt.open.final_key), pkt.open.min_depth, pkt.open.initial_fee_rate), anchorTx,
        TheirCommit(0, CommitmentSpec(Set.empty, pkt.open.initial_fee_rate, initAmountUsMsat = 0,
          initAmountThemMsat = amount * 1000, amountUsMsat = 0, amountThemMsat = amount * 1000),
          theirRevocationHash), theirNextRevocationHash), 'openWaitForCommitSig)

    case (pkt: proto.pkt, params: OurChannelParams, 'openWaitForOpenNoAnchor :: rest)
      if pkt.open != null && pkt.open.anch == proto.open_channel.anchor_offer.WILL_CREATE_ANCHOR =>

      val theirRevocationHash = Tools sha2Bytes pkt.open.revocation_hash
      val theirNextRevocationHash = Tools sha2Bytes pkt.open.next_revocation_hash
      become(WaitForAnchor(params, TheirChannelParams(pkt.open.delay.blocks, proto2ECKey(pkt.open.commit_key),
        proto2ECKey(pkt.open.final_key), pkt.open.min_depth, pkt.open.initial_fee_rate), theirRevocationHash,
        theirNextRevocationHash), 'openWaitForAnchor)

    case (pkt: proto.pkt, WaitForCommitSig(ourParams, theirParams, anchorTx, initialCommit,
      theirNextRevocationHash), 'openWaitForCommitSig :: rest) if pkt.open_commit_sig != null =>

    case (pkt: proto.pkt, WaitForAnchor(ourParams, theirParams, theirRevocationHash,
      theirNextRevocationHash), 'openWaitForAnchor :: rest) if pkt.open_anchor != null =>
      val ourRevocationHash = ShaChain.revIndexFromSeed(ourParams.shaSeed, ShaChain.largestIndex)
      val nonReversedTxHashWrap = Sha256Hash wrap sha2Bytes(pkt.open_anchor.txid)
      val anchorAmount = pkt.open_anchor.amount.longValue

      // Recreate parts of their anchor transaction
      val anchorScript = Scripts pay2wsh Scripts.multiSig2of2(ourParams.commitPubKey, theirParams.commitPubKey)
      val anchorOutput = new TransactionOutput(app.params, null, Coin valueOf anchorAmount, anchorScript.build.getProgram)
      val anchorOutPoint = new TransactionOutPoint(app.params, pkt.open_anchor.output_index.longValue, nonReversedTxHashWrap)
      val ins = new TransactionInput(app.params, null, Array.emptyByteArray, anchorOutPoint, Coin valueOf anchorAmount) :: Nil

      // Initialize parameters and create first pair of commitment transactions
      val ourSpec = CommitmentSpec(Set.empty, ourParams.initialFeeRate, anchorAmount * 1000, 0, anchorAmount * 1000, 0)
      val theirSpec = CommitmentSpec(Set.empty, theirParams.initialFeeRate, 0, anchorAmount * 1000, 0, anchorAmount * 1000)
      val ourTx = Scripts.makeCommitTx(ins, ourParams.finalPubKey, theirParams.finalPubKey, ourParams.delay, ourRevocationHash, ourSpec)
      val theirTx = Scripts.makeCommitTx(ins, theirParams.finalPubKey, ourParams.finalPubKey, theirParams.delay, theirRevocationHash, theirSpec)
      val ourSigForThem = Scripts.signTx(ourParams, theirParams, theirTx, anchorAmount)

      authHandler process new proto.open_commit_sig(ourSigForThem)
      become(Commitments(None, ourParams, theirParams, OurChanges(Vector.empty, Vector.empty, Vector.empty),
        TheirChanges(Vector.empty, Vector.empty), OurCommit(0, ourSpec, ourTx), TheirCommit(0, theirSpec, theirRevocationHash),
        Right(theirNextRevocationHash), anchorOutput, HEX encode nonReversedTxHashWrap.getReversedBytes, None -> Map.empty),
        'openWaitForAnchorConfirm)

    case (something: Any, _, _) =>
      // Let know if received an unhandled message in some state
      println(s"Unhandled $something in Channel at $state : $data")
  }
}
