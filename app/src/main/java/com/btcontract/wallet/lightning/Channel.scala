package com.btcontract.wallet.lightning

import Tools._
import okio.ByteString
import org.bitcoinj.core._
import com.softwaremill.quicklens._
import com.btcontract.wallet.Utils.{app, runAnd}
import org.bitcoinj.core.Utils.HEX
import Scripts.commit2Inputs
import crypto.ShaChain


abstract class Channel(state: List[Symbol], data: ChannelData)
extends StateMachine[ChannelData](state, data) { me =>

  val INVALID_COMMIT_PREIMAGE = "InvalidCommitPreimage"
  val UNKNOWN_HTLC_PREIMAGE = "UnknownHTLCPreimage"
  val INVALID_COMMIT_SIG = "InvalidCommitSig"
  val NOT_ENOUGH_FUNDS = "NotEnoughFunds"
  val UNKNOWN_HTLC_ID = "UnknownHTLCId"
  val authHandler: AuthHandler

  def doProcess(change: Any) = (data, change, state) match {
    case (paramsWithAnchor: OurChannelParams, null, 'inactive :: rest) if paramsWithAnchor.anchorAmount.isDefined =>
      authHandler process paramsWithAnchor.toOpenProto(proto.open_channel.anchor_offer.WILL_CREATE_ANCHOR)
      become(paramsWithAnchor, 'openWaitForOpenWithAnchor)

    case (paramsNoAnchor: OurChannelParams, null, 'inactive :: rest) if paramsNoAnchor.anchorAmount.isEmpty =>
      authHandler process paramsNoAnchor.toOpenProto(proto.open_channel.anchor_offer.WONT_CREATE_ANCHOR)
      become(paramsNoAnchor, 'openWaitForOpenNoAnchor)

    // INIT

    case (params: OurChannelParams, pkt: proto.pkt, 'openWaitForOpenWithAnchor :: rest)
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

    case (params: OurChannelParams, pkt: proto.pkt, 'openWaitForOpenNoAnchor :: rest)
      if pkt.open != null && pkt.open.anch == proto.open_channel.anchor_offer.WILL_CREATE_ANCHOR =>

      val theirRevocationHash = Tools sha2Bytes pkt.open.revocation_hash
      val theirNextRevocationHash = Tools sha2Bytes pkt.open.next_revocation_hash
      become(WaitForAnchor(params, TheirChannelParams(pkt.open.delay.blocks, proto2ECKey(pkt.open.commit_key),
        proto2ECKey(pkt.open.final_key), pkt.open.min_depth, pkt.open.initial_fee_rate), theirRevocationHash,
        theirNextRevocationHash), 'openWaitForAnchor)

    // We have sent an anchor tx info and await for their signature which will let us spend an anchor
    case (WaitForCommitSig(ourParams, theirParams, anchorTx, anchorIdx, theirCommit, theirNextRevocationHash),
      pkt: proto.pkt, 'openWaitForCommitSig :: rest) if pkt.open_commit_sig != null =>

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
      else become(WaitForConfirms(Commitments(ourParams, theirParams, OurChanges(Vector.empty, Vector.empty, Vector.empty),
        TheirChanges(Vector.empty, Vector.empty), OurCommit(0, ourSpec, ourSignedTx), theirCommit, Right(theirNextRevocationHash),
        spendOutput, HEX encode nonReversedTxHashWrap.getReversedBytes), None, depthOk = false), 'openWaitForOurAnchorConfirm)

    // We won't fund an achor so we wait for their anchor info to respond with signature
    case (WaitForAnchor(ourParams, theirParams, theirRevocationHash, theirNextRevocationHash),
      pkt: proto.pkt, 'openWaitForAnchor :: rest) if pkt.open_anchor != null =>

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
      become(WaitForConfirms(Commitments(ourParams, theirParams, OurChanges(Vector.empty, Vector.empty, Vector.empty),
        TheirChanges(Vector.empty, Vector.empty), OurCommit(0, ourSpec, ourTx), TheirCommit(0, theirSpec, theirRevocationHash),
        Right(theirNextRevocationHash), anchorOutput, HEX encode nonReversedTxHashWrap.getReversedBytes),
        None, depthOk = false), 'openWaitForTheirAnchorConfirm)

    // We've got local depth confirmation, now maybe must wait for them
    case (w: WaitForConfirms, (depth: Int, 'anchor), ('openWaitForOurAnchorConfirm |
      'openWaitForTheirAnchorConfirm) :: rest) if depth >= w.commits.ourParams.minDepth =>

      if (w.blockHash.isDefined) become(w.commits, 'normal)
      else me stayWith w.copy(depthOk = true)

    // We've got a confirmation from them, now maybe must wait for local
    case (w: WaitForConfirms, pkt: proto.pkt, ('openWaitForOurAnchorConfirm |
      'openWaitForTheirAnchorConfirm) :: rest) if pkt.open_complete != null =>

      val hash = sha2Bytes(pkt.open_complete.blockid)
      if (w.depthOk) become(fresh = w.commits, 'normal)
      else me stayWith w.copy(blockHash = Some apply hash)

    // When something goes wrong with their anchor while we wait for confirmations
    case (_, pkt: proto.pkt, 'openWaitForTheirAnchorConfirm :: rest)
      if pkt.error != null => become(data, 'closed)

    case (_, 'close | 'anchorSpent | 'anchorTimeOut, 'openWaitForTheirAnchorConfirm :: rest) =>
      handleError("Anchor has been spent or timed out or channel was stopped", 'closed)

    // When they close a channel while we wait for anchor we have to spend from it
    case (w: WaitForConfirms, pkt: proto.pkt, 'openWaitForOurAnchorConfirm :: rest)
      if pkt.error != null => become(w.commits, 'uniclosing)

    // When something goes wrong with our anchor while we wait for confirmations, we have to spend from it
    case (w: WaitForConfirms, 'close | 'anchorSpent | 'anchorTimeOut, 'openWaitForOurAnchorConfirm :: rest) =>
      handleError("Anchor has been spent or timed out or channel was stopped", 'uniclosing)

      // Maybe we don't need this state?
    // Listening to commit tx depth after something went wrong with channel
    case (_, (depth: Int, 'commit), 'uniclosing :: rest) if depth > 2 => become(data, 'closed)

    // MAIN LOOP

    // Send a brand new HTLC to them (make an outgoing payment) if we have enough funds
    case (c: Commitments, HtlcBase(msat, rHash, nextNodeIds, prevChannelId, expiry), 'normal :: rest) =>
      val reduced = c.theirCommit.spec.reduce(c.theirChanges.acked, c.ourChanges.acked ++ c.ourChanges.signed ++ c.ourChanges.proposed)
      val availableFunds = reduced.amountThemMsat + reduced.htlcs.filterNot(_.incoming).map(htlc => -htlc.base.amountMsat).sum
      if (msat > availableFunds) throw new Exception(NOT_ENOUGH_FUNDS) else me stayWith c.modify(_.htlcIndex).using(_ + 1)

    // Receive new HTLC from them only if enough funds
    case (c: Commitments, pkt: proto.pkt, 'normal :: rest) if pkt.update_add_htlc != null =>
      val reduced = c.ourCommit.spec.reduce(c.ourChanges.acked, c.theirChanges.acked ++ c.theirChanges.proposed)
      val availableFunds = reduced.amountThemMsat + reduced.htlcs.filterNot(_.incoming).map(htlc => -htlc.base.amountMsat).sum
      if (pkt.update_add_htlc.amount_msat > availableFunds) handleError(NOT_ENOUGH_FUNDS, 'uniclosing) else stayWith(c addTheirProposal pkt)

    // Send an HTLC fulfill for an HTLC I've sent them before
    case (c: Commitments, (pkt: proto.pkt, 'fulfill), 'normal :: rest) if pkt.update_fulfill_htlc != null =>
      c.findAddHtlcOpt(c.theirChanges.acked, pkt.update_fulfill_htlc.id).map(htlcAdd => htlcAdd.r_hash) match {
        case Some(hash) if r2HashProto(pkt.update_fulfill_htlc.r) == hash => stayRespond(c addOurProposal pkt, pkt)
        case Some(hash) => throw new Exception(UNKNOWN_HTLC_PREIMAGE)
        case _ => throw new Exception(UNKNOWN_HTLC_ID)
      }

    // Receive a fulfill for an HTLC I've got before
    case (c: Commitments, pkt: proto.pkt, 'normal :: rest) if pkt.update_fulfill_htlc != null =>
      c.findAddHtlcOpt(c.theirChanges.acked, pkt.update_fulfill_htlc.id).map(add => add.r_hash) match {
        case Some(hash) if r2HashProto(pkt.update_fulfill_htlc.r) == hash => stayWith(c addTheirProposal pkt)
        case Some(hash) => handleError(UNKNOWN_HTLC_PREIMAGE, 'uniclosing)
        case None => handleError(UNKNOWN_HTLC_ID, 'uniclosing)
      }

    // Send an HTLC fail for an HTLC I've sent them before
    case (c: Commitments, (pkt: proto.pkt, 'fail), 'normal :: rest) if pkt.update_fail_htlc != null =>
      if (c.findAddHtlcOpt(c.theirChanges.acked, pkt.update_fail_htlc.id).isDefined) stayRespond(c addOurProposal pkt, pkt)
      else throw new Exception(UNKNOWN_HTLC_ID)

    // Receive an HTLC fail for an HTLC I've got from them before
    case (c: Commitments, pkt: proto.pkt, 'normal :: rest) if pkt.update_fail_htlc != null =>
      if (c.findAddHtlcOpt(c.ourChanges.acked, pkt.update_fail_htlc.id).isDefined) stayWith(c addTheirProposal pkt)
      else handleError(UNKNOWN_HTLC_ID, 'uniclosing)

    // Send a commitment transaction to them
    case (c: Commitments, 'commit, 'normal :: rest) =>
      c.theirNextCommitInfo.right foreach { theirNextRevocationHash =>
        val spec1 = c.theirCommit.spec.reduce(c.theirChanges.acked, c.ourChanges.acked ++ c.ourChanges.signed ++ c.ourChanges.proposed)
        val ourSigForThem = Scripts.signTx(c.ourParams, c.theirParams, Scripts.makeCommitTx(c.ourCommit, c.theirParams.finalPubKey,
          c.ourParams.finalPubKey, c.theirParams.delay, theirNextRevocationHash, spec1), c.anchorOutput.getValue.value)

        // Their commitment now includes all our changes + their acked changes
        val theirCommit1 = Left apply TheirCommit(c.theirCommit.index + 1, spec1, theirNextRevocationHash)
        val ourChanges1 = c.ourChanges.copy(proposed = Vector.empty, signed = c.ourChanges.signed ++ c.ourChanges.proposed)
        stayRespond(c.copy(theirNextCommitInfo = theirCommit1, ourChanges = ourChanges1), new proto.update_commit.Builder sig ourSigForThem)
      }

    // Receive a commitment transaction from them but only if it's correct
    case (c: Commitments, pkt: proto.pkt, 'normal :: rest) if pkt.update_commit != null =>
      val spec1 = c.ourCommit.spec.reduce(c.ourChanges.acked, c.theirChanges.acked ++ c.theirChanges.proposed)
      val ourNextRevocationHash = Sha256Hash hash ShaChain.revIndexFromSeed(c.ourParams.shaSeed, c.ourCommit.index + 1)
      val ourSignedTx = Scripts.addTheirSigAndSignTx(c.ourParams, c.theirParams, Scripts.makeCommitTx(c.ourCommit, c.ourParams.finalPubKey,
        c.theirParams.finalPubKey, c.ourParams.delay, ourNextRevocationHash, spec1), c.anchorOutput.getValue, pkt.update_commit.sig)

      if (Scripts.isBrokenTransaction(ourSignedTx, c.anchorOutput).isSuccess) {
        // We will respond to them with our revocation preimage and our next revocation hash
        val ourRevocationPreimage = ShaChain.revIndexFromSeed(c.ourParams.shaSeed, c.ourCommit.index)
        val ourNextRevocationHash1 = Sha256Hash hash ShaChain.revIndexFromSeed(c.ourParams.shaSeed, c.ourCommit.index + 2)
        val theirChanges1 = c.theirChanges.copy(proposed = Vector.empty, acked = c.theirChanges.acked ++ c.theirChanges.proposed)
        val revocation = new proto.update_revocation(Tools bytes2Sha ourRevocationPreimage, Tools bytes2Sha ourNextRevocationHash1)
        stayRespond(c.copy(ourCommit = OurCommit(c.ourCommit.index + 1, spec1, ourSignedTx), theirChanges = theirChanges1), revocation)
      } else handleError(INVALID_COMMIT_SIG, 'uniclosing)

    // Receive a revocation in return for our commit
    case (c: Commitments, pkt: proto.pkt, 'normal :: rest) if pkt.update_revocation != null =>
      val preimageHash = preimg2HashProto(pkt.update_revocation.revocation_preimage)
      val isMatch = preimageHash == bytes2Sha(c.theirCommit.theirRevocationHash)

      if (isMatch) c.theirNextCommitInfo.left foreach { theirNextCommit =>
        val commitInfo1 = Right apply sha2Bytes(pkt.update_revocation.next_revocation_hash)
        val changes1 = c.ourChanges.copy(signed = Vector.empty, acked = c.ourChanges.acked ++ c.ourChanges.signed)
        val sha1 = ShaChain.revAddHash(c.theirPreimages, sha2Bytes(pkt.update_revocation.revocation_preimage), c.theirCommit.index)
        me stayWith c.copy(theirPreimages = sha1, ourChanges = changes1, theirNextCommitInfo = commitInfo1, theirCommit = theirNextCommit)
      } else handleError(INVALID_COMMIT_PREIMAGE, 'uniclosing)

    // Send mutual closing request and remember it
    case (c: Commitments, 'close, 'normal :: rest) =>
      val clearingAskedNow = Some apply System.currentTimeMillis
      stayRespond(c.copy(clearingRequested = clearingAskedNow), c.clearing)

    // Receive mutual closing request with active HTLC's
    case (c: Commitments, pkt: proto.pkt, 'normal :: rest)
      if pkt.close_clearing != null && c.hasNoPendingHtlcs =>

      val state = ChannelClearing(c, c.clearing, pkt.close_clearing)
      if (c.clearingRequested.isEmpty) authHandler process c.clearing
      become(state, 'negotiating)

    // Receive mutual closing request with no active HTLC's
    case (c: Commitments, pkt: proto.pkt, 'normal :: rest) if pkt.close_clearing != null =>
      val (_, ourCloseSig) = Scripts.makeFinalTx(c, ourClearing = c.clearing, pkt.close_clearing)
      val state = ChannelFeeNegotiating(ourCloseSig, c.clearing, pkt.close_clearing, c)
      if (c.clearingRequested.isEmpty) authHandler process c.clearing
      authHandler process ourCloseSig
      become(state, 'clearing)

    case (_, something, _) =>
      // Let know if received an unhandled message in some state
      println(s"Unhandled $something in Channel at $state : $data")
  }

  def stayRespond(newData: ChannelData, toThem: Any) =
    runAnd(authHandler process toThem)(me stayWith newData)

  def handleError(reason: String, newState: Symbol) = {
    authHandler.process(new proto.error.Builder problem reason)
    become(data, newState)
  }
}
