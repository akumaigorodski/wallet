package com.btcontract.wallet.lightning

import Tools._
import org.bitcoinj.core._
import com.softwaremill.quicklens._
import com.btcontract.wallet.Utils.{app, runAnd}
import org.bitcoinj.core.Utils.HEX
import crypto.ShaChain
import okio.ByteString


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
      val (anchorTx, anchorIdx) = Scripts.makeAnchorTx(params.commitPubKey, theirCommitPubKey, amount)
      val nonReversedTxHashProto = bytes2Sha(Sha256Hash hashTwice anchorTx.unsafeBitcoinSerialize)
      val theirCommit = TheirCommit(0, CommitmentSpec(Set.empty, pkt.open.initial_fee_rate,
        initAmountUsMsat = 0, initAmountThemMsat = amount * 1000, amountUsMsat = 0,
        amountThemMsat = amount * 1000), theirRevocationHash)

      authHandler process new proto.open_anchor(nonReversedTxHashProto, anchorIdx, amount)
      become(WaitForCommitSig(params, TheirChannelParams(pkt.open.delay.blocks, theirCommitPubKey,
        proto2ECKey(pkt.open.final_key), pkt.open.min_depth, pkt.open.initial_fee_rate), anchorTx,
        anchorIdx, theirCommit, theirNextRevocationHash), 'openWaitForCommitSig)

    case (pkt: proto.pkt, params: OurChannelParams, 'openWaitForOpenNoAnchor :: rest)
      if pkt.open != null && pkt.open.anch == proto.open_channel.anchor_offer.WILL_CREATE_ANCHOR =>

      val theirRevocationHash = Tools sha2Bytes pkt.open.revocation_hash
      val theirNextRevocationHash = Tools sha2Bytes pkt.open.next_revocation_hash
      become(WaitForAnchor(params, TheirChannelParams(pkt.open.delay.blocks, proto2ECKey(pkt.open.commit_key),
        proto2ECKey(pkt.open.final_key), pkt.open.min_depth, pkt.open.initial_fee_rate), theirRevocationHash,
        theirNextRevocationHash), 'openWaitForAnchor)

    case (pkt: proto.pkt, WaitForCommitSig(ourParams, theirParams, anchorTx, anchorIdx, theirCommit,
      theirNextRevocationHash), 'openWaitForCommitSig :: rest) if pkt.open_commit_sig != null =>

      val anchorAmount = anchorTx.getOutput(anchorIdx).getValue
      val nonReversedTxHashWrap = Sha256Hash twiceOf anchorTx.unsafeBitcoinSerialize
      val ourRevocationHash = ShaChain.revIndexFromSeed(ourParams.shaSeed, 0)

      // Fully sign our transaction, ourSpec is theirSpec in 'openWaitForAnchor'
      val anchorOutPoint = new TransactionOutPoint(app.params, anchorIdx, nonReversedTxHashWrap)
      val ins = new TransactionInput(app.params, null, Array.emptyByteArray, anchorOutPoint, anchorAmount) :: Nil
      val ourSpec = CommitmentSpec(Set.empty, ourParams.initialFeeRate, 0, anchorAmount.getValue * 1000, 0, anchorAmount.getValue * 1000)
      val ourTx = Scripts.makeCommitTx(ins, ourParams.finalPubKey, theirParams.finalPubKey, ourParams.delay, ourRevocationHash, ourSpec)
      val ourSignedTx = Scripts.addTheirSigAndSignTx(ourParams, theirParams, ourTx, anchorAmount, pkt.open_commit_sig.sig)

      val spendOutput = ourSignedTx getOutput anchorIdx
      if (Scripts.isBrokenTransaction(ourSignedTx, spendOutput).isFailure) handleError("Bad signature", 'closed)
      else become(WaitForConfirms(Commitments(None, ourParams, theirParams, OurChanges(Vector.empty, Vector.empty, Vector.empty),
        TheirChanges(Vector.empty, Vector.empty), OurCommit(0, ourSpec, ourSignedTx), theirCommit, Right(theirNextRevocationHash),
        spendOutput, HEX encode nonReversedTxHashWrap.getReversedBytes), None, depthOk = false), 'openWaitForOurAnchorConfirm)

    case (pkt: proto.pkt, WaitForAnchor(ourParams, theirParams, theirRevocationHash,
      theirNextRevocationHash), 'openWaitForAnchor :: rest) if pkt.open_anchor != null =>

      // ourTx is just a placeholder since we can't spend it anyway
      val nonReversedTxHashWrap = Sha256Hash wrap sha2Bytes(pkt.open_anchor.txid)
      val anchorAmount = pkt.open_anchor.amount.longValue
      val ourTx = new Transaction(app.params)

      // Recreate parts of their anchor transaction
      val anchorScript = Scripts pay2wsh Scripts.multiSig2of2(ourParams.commitPubKey, theirParams.commitPubKey)
      val anchorOutput = new TransactionOutput(app.params, null, Coin valueOf anchorAmount, anchorScript.build.getProgram)
      val anchorOutPoint = new TransactionOutPoint(app.params, pkt.open_anchor.output_index.longValue, nonReversedTxHashWrap)
      val ins = new TransactionInput(app.params, null, Array.emptyByteArray, anchorOutPoint, Coin valueOf anchorAmount) :: Nil

      // Initialize parameters and sign their commitment transaction
      val ourSpec = CommitmentSpec(Set.empty, ourParams.initialFeeRate, anchorAmount * 1000, 0, anchorAmount * 1000, 0)
      val theirSpec = CommitmentSpec(Set.empty, theirParams.initialFeeRate, 0, anchorAmount * 1000, 0, anchorAmount * 1000)
      val ourSigForThem = Scripts.signTx(ourParams, theirParams, Scripts.makeCommitTx(ins, theirParams.finalPubKey,
        ourParams.finalPubKey, theirParams.delay, theirRevocationHash, theirSpec), anchorAmount)

      authHandler process new proto.open_commit_sig(ourSigForThem)
      become(WaitForConfirms(Commitments(None, ourParams, theirParams, OurChanges(Vector.empty, Vector.empty, Vector.empty),
        TheirChanges(Vector.empty, Vector.empty), OurCommit(0, ourSpec, ourTx), TheirCommit(0, theirSpec, theirRevocationHash),
        Right(theirNextRevocationHash), anchorOutput, HEX encode nonReversedTxHashWrap.getReversedBytes),
        None, depthOk = false), 'openWaitForTheirAnchorConfirm)

    case (Tuple2(depth: Int, 'anchor), w: WaitForConfirms, ('openWaitForOurAnchorConfirm | 'openWaitForTheirAnchorConfirm) :: rest)
      if depth >= w.commits.ourParams.minDepth => if (w.blockHash.isDefined) become(w.commits, 'normal) else me stayWith w.copy(depthOk = true)

    case (pkt: proto.pkt, w: WaitForConfirms, ('openWaitForOurAnchorConfirm | 'openWaitForTheirAnchorConfirm) :: rest)
      if pkt.open_complete != null => if (w.depthOk) become(w.commits, 'normal) else me stayWith w.withHash(pkt.open_complete.blockid)

    // When something goes wrong with their anchor while we wait for confirmations
    case (pkt: proto.pkt, _, 'openWaitForTheirAnchorConfirm :: rest) if pkt.error != null => become(data, 'closed)
    case ('close | 'anchorSpent | 'anchorTimeOut, _, 'openWaitForTheirAnchorConfirm :: rest) =>
      handleError("Anchor has been spent or timed out or channel was stopped", 'closed)

    // When they close a channel while we wait for anchor we have to spend from it
    case (pkt: proto.pkt, w: WaitForConfirms, 'openWaitForOurAnchorConfirm :: rest)
      if pkt.error != null => become(w.commits, 'closingAnchor)

    // When something goes wrong with our anchor while we wait for confirmations, we have to spend from it
    case ('close | 'anchorSpent | 'anchorTimeOut, w: WaitForConfirms, 'openWaitForOurAnchorConfirm :: rest) =>
      handleError("Anchor has been spent or timed out or channel was stopped", 'closingAnchor)

    // Listening to commit tx depth after something went wrong with channel
    case (Tuple2(depth: Int, 'commit), c: Commitments, 'closingAnchor :: rest)
      if depth >= c.ourParams.minDepth => become(c, 'closed)

    // MAIN LOOP

    // Send a brand new HTLC to them if we have enough funds
    case (HtlcBase(msat, rHash, nextNodeIds, prevChannelId, expiry), c: Commitments, 'normal :: rest) =>
      val reduced = c.theirCommit.spec.reduce(c.theirChanges.acked, c.ourChanges.acked ++ c.ourChanges.signed ++ c.ourChanges.proposed)
      val availableFunds = reduced.amountThemMsat + reduced.htlcs.filterNot(_.incoming).map(htlc => -htlc.base.amountMsat).sum

      if (msat > availableFunds) handleError("Insufficient funds", 'closingAnchor) else {
        val routing = new proto.routing.Builder info ByteString.of(new proto.route(null).encode:_*)
        val addProto = new proto.update_add_htlc(c.htlcIndex, msat, bytes2Sha(rHash), blocks(expiry), routing.build)
        stayRespond(c.addOurProposal(Tools toPkt addProto).copy(htlcIndex = c.htlcIndex + 1), addProto)
      }

    // Receive new HTLC from them only if enough funds
    case (pkt: proto.pkt, c: Commitments, 'normal :: rest)
      if pkt.update_add_htlc != null =>

      val reduced = c.ourCommit.spec.reduce(c.ourChanges.acked, c.theirChanges.acked ++ c.theirChanges.proposed)
      val availableFunds = reduced.amountThemMsat + reduced.htlcs.filterNot(_.incoming).map(-_.base.amountMsat).sum
      if (pkt.update_add_htlc.amount_msat > availableFunds) handleError("Insufficient funds", 'closingAnchor)
      else stayWith(c addTheirProposal pkt)

    // Send an HTLC fulfill for an HTLC I've sent them before
    case (Tuple2(pkt: proto.pkt, 'send), c: Commitments, 'normal :: rest)
      if pkt.update_fulfill_htlc != null =>

      c.theirChanges.acked.find(c htlcId pkt.update_fulfill_htlc.id).map(_.update_add_htlc.r_hash) match {
        case Some(hash) if r2HashProto(pkt.update_fulfill_htlc.r) == hash => stayRespond(c addOurProposal pkt, pkt)
        case Some(hash) => handleError("Invalid HTLC preimage", 'normal)
        case _ => handleError("Unknown HTLC id", 'normal)
      }

    // Receive a fulfill for an HTLC I've got before
    case (pkt: proto.pkt, c: Commitments, 'normal :: rest)
      if pkt.update_fulfill_htlc != null =>

      c.ourChanges.acked.find(c htlcId pkt.update_fulfill_htlc.id).map(_.update_add_htlc.r_hash) match {
        case Some(hash) if r2HashProto(pkt.update_fulfill_htlc.r) == hash => stayWith(c addTheirProposal pkt)
        case Some(hash) => handleError("Invalid HTLC preimage", 'closingAnchor)
        case None => handleError("Unknown HTLC id", 'closingAnchor)
      }

    case (something: Any, _, _) =>
      // Let know if received an unhandled message in some state
      println(s"Unhandled $something in Channel at $state : $data")
  }

  def stayRespond(newData: ChannelData, toThem: Any) =
    runAnd(authHandler process toThem)(me stayWith newData)

  def handleError(why: String, state: Symbol) = {
    val error = new proto.error.Builder problem why
    authHandler process error.build
    become(data, state)
  }
}
