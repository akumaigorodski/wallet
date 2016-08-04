package com.btcontract.wallet.lightning

import Tools._
import org.bitcoinj.core._
import com.softwaremill.quicklens._
import com.btcontract.wallet.Utils.app
import org.bitcoinj.core.Utils.HEX
import Scripts.commit2Inputs
import crypto.ShaChain


abstract class Channel(state: List[Symbol], data: ChannelData)
extends StateMachine[ChannelData](state, data) { me =>

  val INVALID_COMMIT_PREIMAGE = "InvalidCommitPreimage"
  val UNKNOWN_HTLC_PREIMAGE = "UnknownHTLCPreimage"
  val INVALID_CLOSING_SIG = "InvalidClosingSig"
  val INVALID_COMMIT_SIG = "InvalidCommitSig"
  val NOT_ENOUGH_FUNDS = "NotEnoughFunds"
  val UNKNOWN_HTLC_ID = "UnknownHTLCId"
  val authHandler: AuthHandler

  def doProcess(change: Any) = (data, change, state) match {
    case (paramsWithAnchor: OurChannelParams, null, 'INACTIVE :: rest) if paramsWithAnchor.anchorAmount.isDefined =>
      authHandler process paramsWithAnchor.toOpenProto(proto.open_channel.anchor_offer.WILL_CREATE_ANCHOR)
      become(paramsWithAnchor, 'OPEN_WAIT_OPEN_WITH_ANCHOR)

    case (paramsNoAnchor: OurChannelParams, null, 'INACTIVE :: rest) if paramsNoAnchor.anchorAmount.isEmpty =>
      authHandler process paramsNoAnchor.toOpenProto(proto.open_channel.anchor_offer.WONT_CREATE_ANCHOR)
      become(paramsNoAnchor, 'OPEN_WAIT_OPEN_NO_ANCHOR)

    // INIT

    case (params: OurChannelParams, pkt: proto.pkt, 'OPEN_WAIT_OPEN_WITH_ANCHOR :: rest)
      if has(pkt.open) && pkt.open.anch == proto.open_channel.anchor_offer.WONT_CREATE_ANCHOR =>

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
        anchorIdx, theirCommit, theirNextRevocationHash), 'OPEN_WAIT_COMMIT_SIG)

    case (params: OurChannelParams, pkt: proto.pkt, 'OPEN_WAIT_OPEN_NO_ANCHOR :: rest)
      if has(pkt.open) && pkt.open.anch == proto.open_channel.anchor_offer.WILL_CREATE_ANCHOR =>

      val theirRevocationHash = Tools sha2Bytes pkt.open.revocation_hash
      val theirNextRevocationHash = Tools sha2Bytes pkt.open.next_revocation_hash
      become(WaitForAnchor(params, TheirChannelParams(pkt.open.delay.blocks, proto2ECKey(pkt.open.commit_key),
        proto2ECKey(pkt.open.final_key), pkt.open.min_depth, pkt.open.initial_fee_rate), theirRevocationHash,
        theirNextRevocationHash), 'OPEN_WAIT_ANCHOR)

    // We have sent an anchor tx info and await for their signature which will let us spend an anchor
    case (WaitForCommitSig(ourParams, theirParams, anchorTx, anchorIdx, theirCommit, theirNextRevocationHash),
      pkt: proto.pkt, 'OPEN_WAIT_COMMIT_SIG :: rest) if has(pkt.open_commit_sig) =>

      val anchorAmount = anchorTx.getOutput(anchorIdx).getValue
      val nonReversedTxHashWrap = Sha256Hash twiceOf anchorTx.unsafeBitcoinSerialize
      val ourRevocationHash = ShaChain.revIndexFromSeed(ourParams.shaSeed, 0)

      // Fully sign our transaction, ourSpec is theirSpec in 'openWaitForAnchor'
      val anchorOutPoint = new TransactionOutPoint(app.params, anchorIdx, nonReversedTxHashWrap)
      val ins = new TransactionInput(app.params, null, Array.emptyByteArray, anchorOutPoint, anchorAmount) :: Nil
      val ourSpec = CommitmentSpec(Set.empty, ourParams.initialFeeRate, 0, anchorAmount.value * 1000, 0, anchorAmount.value * 1000)
      val ourTx = Scripts.makeCommitTx(ins, ourParams.finalPubKey, theirParams.finalPubKey, ourParams.delay, ourRevocationHash, ourSpec)
      val ourSignedTx = Scripts.addTheirSigAndSignTx(ourParams, theirParams, ourTx, anchorAmount.value, pkt.open_commit_sig.sig)

      val ourOutput = ourSignedTx getOutput anchorIdx
      if (Scripts.brokenTxCheck(ourSignedTx, ourOutput).isFailure) handleError("Bad signature", 'CLOSED)
      else become(WaitForConfirms(Commitments(ourParams, theirParams, OurChanges(Vector.empty, Vector.empty, Vector.empty),
        TheirChanges(Vector.empty, Vector.empty), OurCommit(0, ourSpec, ourSignedTx), theirCommit, Right(theirNextRevocationHash),
        ourOutput, HEX encode nonReversedTxHashWrap.getReversedBytes), blockHash = None, depthOk = false), 'OPEN_WAIT_OUR_ANCHOR_CONFIRM)

    // We won't fund an achor so we wait for their anchor info to respond with signature
    case (WaitForAnchor(ourParams, theirParams, theirRevocationHash, theirNextRevocationHash),
      pkt: proto.pkt, 'OPEN_WAIT_ANCHOR :: rest) if has(pkt.open_anchor) =>

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
        blockHash = None, depthOk = false), 'OPEN_WAIT_THEIR_ANCHOR_CONFIRM)

    // We've got local depth confirmation, now maybe must wait for them
    case (w: WaitForConfirms, (depth: Int, 'ANCHOR), ('OPEN_WAIT_OUR_ANCHOR_CONFIRM |
      'OPEN_WAIT_THEIR_ANCHOR_CONFIRM) :: rest) if depth >= w.commits.ourParams.minDepth =>

      if (w.blockHash.isDefined) become(w.commits, 'NORMAL)
      else me stayWith w.copy(depthOk = true)

    // We've got a confirmation from them, now maybe must wait for local
    case (w: WaitForConfirms, pkt: proto.pkt, ('OPEN_WAIT_OUR_ANCHOR_CONFIRM |
      'OPEN_WAIT_THEIR_ANCHOR_CONFIRM) :: rest) if has(pkt.open_complete) =>

      val hash = sha2Bytes(pkt.open_complete.blockid)
      if (w.depthOk) become(fresh = w.commits, 'NORMAL)
      else me stayWith w.copy(blockHash = Some apply hash)

    // When something goes wrong with their anchor while we wait for confirmations
    case (_, pkt: proto.pkt, 'OPEN_WAIT_THEIR_ANCHOR_CONFIRM :: rest)
      if has(pkt.error) => become(data, 'CLOSED)

    // When something goes wrong with their anchor
    case (_, 'UNICLOSE, 'OPEN_WAIT_THEIR_ANCHOR_CONFIRM :: rest) =>
      handleError("Anchor has been spent or timed out", 'CLOSED)

    // When they close a channel while we wait for anchor we have to spend from it
    case (w: WaitForConfirms, pkt: proto.pkt, 'OPEN_WAIT_OUR_ANCHOR_CONFIRM :: rest)
      if has(pkt.error) => become(w.commits, 'UNICLOSING)

    // When something goes wrong with our anchor
    case (_, 'UNICLOSE, ('OPEN_WAIT_OUR_ANCHOR_CONFIRM | 'NORMAL) :: rest) =>
      handleError("Anchor has been spent or timed out", 'UNICLOSING)

    // Listening to commit or final transaction depth
    case (_, depth: Int, ('UNICLOSING | 'CLOSING) :: rest)
      if depth > 1 => become(data, 'CLOSED)

    // MAIN LOOP

    // Send a brand new HTLC to them (make an outgoing payment) if we have enough funds and not in a clearing process
    case (c: Commitments, HtlcBase(msat, rHash, nextNodeIds, prevChannelId, expiry), 'NORMAL :: rest) if c.clearingStarted.isEmpty =>
      val reduced = c.theirCommit.spec.reduce(c.theirChanges.acked, c.ourChanges.acked ++ c.ourChanges.signed ++ c.ourChanges.proposed)
      val availableFunds = reduced.amountThemMsat + reduced.htlcs.filterNot(_.incoming).map(htlc => - htlc.base.amountMsat).sum
      if (msat > availableFunds) throw new Exception(NOT_ENOUGH_FUNDS) else me stayWith c.modify(_.htlcIndex).using(_ + 1)

    // Receive a brand new HTLC from them
    case (c: Commitments, pkt: proto.pkt, 'NORMAL :: rest) if has(pkt.update_add_htlc) =>
      val reduced = c.ourCommit.spec.reduce(c.ourChanges.acked, c.theirChanges.acked ++ c.theirChanges.proposed)
      val availableFunds = reduced.amountThemMsat + reduced.htlcs.filterNot(_.incoming).map(htlc => - htlc.base.amountMsat).sum
      if (pkt.update_add_htlc.amount_msat > availableFunds) handleError(NOT_ENOUGH_FUNDS, 'UNICLOSING) else stayWith(c addTheirProposal pkt)

    // Send an HTLC fulfill for an HTLC I've sent to them before
    case (c: Commitments, fulfill: proto.update_fulfill_htlc, 'NORMAL :: rest) =>
      c.findAddHtlcOpt(packets = c.theirChanges.acked, id = fulfill.id).map(add => add.r_hash) match {
        case Some(hash) if r2HashProto(fulfill.r) == hash => stayRespond(c addOurProposal toPkt(fulfill), fulfill)
        case Some(hash) => throw new Exception(UNKNOWN_HTLC_PREIMAGE)
        case _ => throw new Exception(UNKNOWN_HTLC_ID)
      }

    // Receive a fulfill for an HTLC I've got from them before
    case (c: Commitments, pkt: proto.pkt, 'NORMAL :: rest) if has(pkt.update_fulfill_htlc) =>
      c.findAddHtlcOpt(c.theirChanges.acked, pkt.update_fulfill_htlc.id).map(add => add.r_hash) match {
        case Some(hash) if r2HashProto(pkt.update_fulfill_htlc.r) == hash => stayWith(c addTheirProposal pkt)
        case Some(hash) => handleError(UNKNOWN_HTLC_PREIMAGE, 'UNICLOSING)
        case None => handleError(UNKNOWN_HTLC_ID, 'UNICLOSING)
      }

    // Send an HTLC fail for an HTLC they have previously sent to me
    case (c: Commitments, fail: proto.update_fail_htlc, 'NORMAL :: rest) =>
      val addHtlcPresent = c.findAddHtlcOpt(packets = c.theirChanges.acked, id = fail.id).isDefined
      if (addHtlcPresent) stayRespond(c addOurProposal toPkt(fail), fail) else throw new Exception(UNKNOWN_HTLC_ID)

    // Receive an HTLC fail for an HTLC I have previously sent to them
    case (c: Commitments, pkt: proto.pkt, 'NORMAL :: rest) if has(pkt.update_fail_htlc) =>
      val addHtlcPresent = c.findAddHtlcOpt(c.ourChanges.acked, pkt.update_fail_htlc.id).isDefined
      if (addHtlcPresent) stayWith(c addTheirProposal pkt) else handleError(UNKNOWN_HTLC_ID, 'UNICLOSING)

    // Send a commitment transaction to them
    case (c: Commitments, 'COMMIT, 'NORMAL :: rest) =>
      c.theirNextCommitInfo.right foreach { theirNextRevocationHash =>
        val spec1 = c.theirCommit.spec.reduce(c.theirChanges.acked, c.ourChanges.acked ++ c.ourChanges.signed ++ c.ourChanges.proposed)
        val ourSigForThem = Scripts.signTx(c.ourParams, c.theirParams, Scripts.makeCommitTx(c.ourCommit, c.theirParams.finalPubKey,
          c.ourParams.finalPubKey, c.theirParams.delay, theirNextRevocationHash, spec1), c.anchorOutput.getValue.value)

        val protoCommit = new proto.update_commit(ourSigForThem)
        val theirCommit1 = Left apply TheirCommit(c.theirCommit.index + 1, spec1, theirNextRevocationHash)
        val oc1 = c.ourChanges.copy(proposed = Vector.empty, signed = c.ourChanges.signed ++ c.ourChanges.proposed)
        stayRespond(c.copy(theirNextCommitInfo = theirCommit1, ourChanges = oc1), protoCommit)
      }

    // Receive a commitment transaction from them if it is correct
    case (c: Commitments, pkt: proto.pkt, 'NORMAL :: rest) if has(pkt.update_commit) =>
      val spec1 = c.ourCommit.spec.reduce(c.ourChanges.acked, c.theirChanges.acked ++ c.theirChanges.proposed)
      val ourNextRevocationHash = Sha256Hash hash ShaChain.revIndexFromSeed(c.ourParams.shaSeed, c.ourCommit.index + 1)
      val ourSignedTx = Scripts.addTheirSigAndSignTx(c.ourParams, c.theirParams, Scripts.makeCommitTx(c.ourCommit, c.ourParams.finalPubKey,
        c.theirParams.finalPubKey, c.ourParams.delay, ourNextRevocationHash, spec1), c.anchorOutput.getValue.value, pkt.update_commit.sig)

      if (Scripts.brokenTxCheck(ourSignedTx, c.anchorOutput).isSuccess) {
        // We will respond to them with our revocation preimage and our next revocation hash
        val ourRevocationPreimage = ShaChain.revIndexFromSeed(c.ourParams.shaSeed, c.ourCommit.index)
        val ourNextRevocationHash1 = Sha256Hash hash ShaChain.revIndexFromSeed(c.ourParams.shaSeed, c.ourCommit.index + 2)
        val theirChanges1 = c.theirChanges.copy(proposed = Vector.empty, acked = c.theirChanges.acked ++ c.theirChanges.proposed)
        val protoRevocation = new proto.update_revocation(Tools bytes2Sha ourRevocationPreimage, Tools bytes2Sha ourNextRevocationHash1)
        stayRespond(c.copy(ourCommit = OurCommit(c.ourCommit.index + 1, spec1, ourSignedTx), theirChanges = theirChanges1), protoRevocation)
      } else handleError(INVALID_COMMIT_SIG, 'UNICLOSING)

    // Receive a revocation in return for our commit
    case (c: Commitments, pkt: proto.pkt, 'NORMAL :: rest) if has(pkt.update_revocation) =>
      val preimageHash = preimg2HashProto(pkt.update_revocation.revocation_preimage)
      val isMatch = preimageHash == bytes2Sha(c.theirCommit.theirRevocationHash)

      if (isMatch) c.theirNextCommitInfo.left foreach { theirNextCommit =>
        val commitInfo1 = Right apply sha2Bytes(pkt.update_revocation.next_revocation_hash)
        val changes1 = c.ourChanges.copy(signed = Vector.empty, acked = c.ourChanges.acked ++ c.ourChanges.signed)
        val chain1 = ShaChain.revAddHash(c.theirPreimages, sha2Bytes(pkt.update_revocation.revocation_preimage), c.theirCommit.index)
        me stayWith c.copy(theirPreimages = chain1, ourChanges = changes1, theirNextCommitInfo = commitInfo1, theirCommit = theirNextCommit)
      } else handleError(INVALID_COMMIT_PREIMAGE, 'UNICLOSING)

    // Reacting to error messages andmutual close commands
    case (c: Commitments, 'CLOSE, 'NORMAL :: rest) if c.clearingStarted.isEmpty => me mutualCloseRespond c
    case (c: Commitments, pkt: proto.pkt, 'NORMAL :: rest) if has(pkt.close_clearing) => me mutualCloseRespond c
    case (c: Commitments, pkt: proto.pkt, 'NORMAL :: rest) if has(pkt.error) => become(c, 'UNICLOSING)

    // Fee negotiations
    case (c: Commitments, pkt: proto.pkt, 'NORMAL :: rest)
      if has(pkt.close_signature) && c.clearingStarted.isDefined =>

      c checkCloseSig pkt.close_signature match {
        case (true, finalTx, ourSig) if pkt.close_signature.close_fee == ourSig.close_fee => become(WaitForClosing(c, finalTx), 'CLOSING)
        case (true, _, ourSig) => authHandler process c.makeNewFeeFinalSig(pkt.close_signature.close_fee, ourSig.close_fee)
        case _ => handleError(INVALID_CLOSING_SIG, 'UNICLOSING)
      }

    case (_, something, _) =>
      // Let know if received an unhandled message in some state
      println(s"Unhandled $something in Channel at $state : $data")
  }

  def mutualCloseRespond(c: Commitments) = {
    val message = new proto.close_clearing(Tools bytes2bs c.ourSciptPubKey.getProgram)
    val c1 = c.copy(clearingStarted = Some apply System.currentTimeMillis)
    stayRespond(c1, message)
  }

  def stayRespond(newData: ChannelData, toThem: Any) = {
    // Method for responding and staying in a current state
    authHandler process toThem
    stayWith(newData)
  }

  def handleError(reason: String, newState: Symbol) = {
    // Method for sending an error and entering a new state
    authHandler process new proto.error(reason)
    become(data, newState)
  }
}