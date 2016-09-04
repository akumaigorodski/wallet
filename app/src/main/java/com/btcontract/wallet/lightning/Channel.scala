package com.btcontract.wallet.lightning

import Tools._
import StateMachine._
import org.bitcoinj.core._
import com.softwaremill.quicklens._

import com.btcontract.wallet.Utils.{Bytes, app}
import collection.JavaConverters.asScalaBufferConverter
import org.bitcoinj.core.Utils.HEX
import crypto.ShaChain


object StateMachine {
  val NOT_ENOUGH_FUNDS = "Not enough funds"
  val CHANNEL_CANCELLED = "Channed has been cancelled"
  val UNKNOWN_HTLC_PREIMAGE = "Unknown Htlc preimage"
  val UNKNOWN_HTLC_ID = "Unknown Htlc id"
}

abstract class Channel(state: List[Symbol], data: ChannelData)
extends StateMachine[ChannelData](state, data) { me =>

  val authHandler: AuthHandler

  def doProcess(change: Any) = (data, change, state.head) match {
    case (null, paramsWithAnchor: OurChannelParams, 'Inactive) if paramsWithAnchor.anchorAmount.isDefined =>
      authHandler process paramsWithAnchor.toOpenProto(proto.open_channel.anchor_offer.WILL_CREATE_ANCHOR)
      become(paramsWithAnchor, 'InitWaitOpenWithAnchor)

    case (null, paramsNoAnchor: OurChannelParams, 'Inactive) if paramsNoAnchor.anchorAmount.isEmpty =>
      authHandler process paramsNoAnchor.toOpenProto(proto.open_channel.anchor_offer.WONT_CREATE_ANCHOR)
      become(paramsNoAnchor, 'InitWaitOpenNoAnchor)

    // INIT

    case (params: OurChannelParams, pkt: proto.pkt, 'InitWaitOpenWithAnchor)
      if has(pkt.open) && pkt.open.anch == proto.open_channel.anchor_offer.WONT_CREATE_ANCHOR =>

      // We will fund an anchor so we will own the funds
      val theirCommitPubKey = proto2ECKey(pkt.open.commit_key)
      val anchor = Scripts.makeAnchorTx(params, theirCommitPubKey)

      // This is *our* view of *their* view of *their* spec so "us" is "them" here
      val theirSpec = CommitmentSpec(Set.empty, pkt.open.initial_fee_rate, initAmountUsMsat = 0,
        initAmountThemMsat = anchor.value * 1000, amountUsMsat = 0, amountThemMsat = anchor.value * 1000)

      authHandler process new proto.open_anchor(bytes2Sha(anchor.hash.getBytes), anchor.idx, anchor.value)
      become(WaitForCommitSig(params, TheirChannelParams(pkt.open.delay.blocks, theirCommitPubKey, proto2ECKey(pkt.open.final_key),
        pkt.open.min_depth, pkt.open.initial_fee_rate), anchor, TheirCommit(index = 0, theirSpec, pkt.open.revocation_hash),
        pkt.open.next_revocation_hash), 'OpenWaitCommitSig)

    case (params: OurChannelParams, pkt: proto.pkt, 'InitWaitOpenNoAnchor)
      if has(pkt.open) && pkt.open.anch == proto.open_channel.anchor_offer.WILL_CREATE_ANCHOR =>

      become(WaitForAnchor(params, TheirChannelParams(pkt.open.delay.blocks, proto2ECKey(pkt.open.commit_key),
        proto2ECKey(pkt.open.final_key), pkt.open.min_depth, pkt.open.initial_fee_rate), pkt.open.revocation_hash,
        pkt.open.next_revocation_hash), 'OpenWaitAnchor)

    // We have sent an anchor tx info and wait for their signature of our commit tx
    case (WaitForCommitSig(ourParams, theirParams, anchor, theirCommit, theirNextRevHash),
      pkt: proto.pkt, 'OpenWaitCommitSig) if has(pkt.open_commit_sig) =>

      // We will fund an anchor so we will own the funds
      val ourRevHash = ShaChain.revIndexFromSeed(ourParams.shaSeed, idx = 0)
      val anchorScript = Scripts pay2wsh Scripts.multiSig2of2(ourParams.commitPubKey, theirParams.commitPubKey)
      val ourSpec = CommitmentSpec(Set.empty, ourParams.initialFeeRate, initAmountUsMsat = anchor.value * 1000,
        initAmountThemMsat = 0, amountUsMsat = anchor.value * 1000, amountThemMsat = 0)

      // Here we create a first commit tx, add their signature and check if resulting tx is valid
      // This transaction also contains an anchor spending input which we will be reusing in each new commit tx
      val template = Scripts.makeCommitTxTemplate(ourParams.finalPubKey, theirParams.finalPubKey, ourParams.delay, ourRevHash, ourSpec)
      val ourSignedCommitTx = Scripts.addTheirSigAndSignTx(ourParams, theirParams, template makeTx new TransactionInput(app.params, null,
        anchorScript.build.getProgram, new TransactionOutPoint(app.params, anchor.idx, anchor.hash), anchor.output.getValue),
        anchor.value, pkt.open_commit_sig.sig)

      if (Scripts.brokenTxCheck(ourSignedCommitTx, anchor.output).isFailure) become(null, 'Closed)
      else become(WaitForConfirms(Commitments(ourParams, theirParams, OurChanges(Vector.empty, Vector.empty, Vector.empty),
        TheirChanges(Vector.empty, Vector.empty), OurCommit(0, ourSpec, ourSignedCommitTx), theirCommit, Right(theirNextRevHash),
        anchor.output, anchor.id), theyConfirmed = false, depthOk = false), 'OpenWaitOurAnchorConfirm)

    // We won't fund an achor so we wait for their anchor info to respond with signature
    case (WaitForAnchor(ourParams, theirParams, theirRevHash, theirNextRevHash),
      pkt: proto.pkt, 'OpenWaitAnchor) if has(pkt.open_anchor) =>

      val anchorAmount = pkt.open_anchor.amount.longValue
      val anchorTxHash = Sha256Hash wrap sha2Bytes(pkt.open_anchor.txid)
      val ourSpec = CommitmentSpec(Set.empty, ourParams.initialFeeRate, initAmountUsMsat = 0,
        initAmountThemMsat = anchorAmount * 1000, amountUsMsat = 0, amountThemMsat = anchorAmount * 1000)

      // They fund an anchor so funds belong to them at start, this is *our* view of *their* view of *their* spec
      val theirSpec = CommitmentSpec(Set.empty, theirParams.initialFeeRate, initAmountUsMsat = anchorAmount * 1000,
        initAmountThemMsat = 0, amountUsMsat = anchorAmount * 1000, amountThemMsat = 0)

      // We create their first commit tx as we see it, we'll also save it as our first commit tx
      // Even though we can't spend their half-signed commit, we'll be reusing it's anchor spending input
      val anchorScript = Scripts pay2wsh Scripts.multiSig2of2(ourParams.commitPubKey, theirParams.commitPubKey)
      val theirFirstCommitTx = Scripts.makeCommitTxTemplate(theirParams.finalPubKey, ourParams.finalPubKey, theirParams.delay,
        sha2Bytes(theirRevHash), theirSpec) makeTx new TransactionInput(app.params, null, anchorScript.build.getProgram,
        new TransactionOutPoint(app.params, pkt.open_anchor.output_index.longValue, anchorTxHash),
        Coin valueOf anchorAmount)

      // We half-sign their commit tx and provide them as signature so they can spend an anchor
      val ourSigForThem = Scripts.signTx(ourParams, theirParams, theirFirstCommitTx, anchorAmount)

      authHandler process new proto.open_commit_sig(ourSigForThem)
      become(WaitForConfirms(Commitments(ourParams, theirParams, OurChanges(Vector.empty, Vector.empty, Vector.empty),
        TheirChanges(Vector.empty, Vector.empty), OurCommit(0, ourSpec, theirFirstCommitTx), TheirCommit(0, theirSpec, theirRevHash),
        Right(theirNextRevHash), new TransactionOutput(app.params, null, Coin valueOf anchorAmount, anchorScript.build.getProgram),
        HEX encode anchorTxHash.getReversedBytes), theyConfirmed = false, depthOk = false), 'OpenWaitTheirAnchorConfirm)

    // AWAITING ANCHOR

    // We've got local depth confirmation, inform them and maybe wait for them
    case (wfc @ WaitForConfirms(commits, theyConfirmedAlready, false), (depth: Int, anchorBlockId: Bytes),
      'OpenWaitOurAnchorConfirm | 'OpenWaitTheirAnchorConfirm) if depth >= commits.ourParams.minDepth =>

      val completeProto = new proto.open_complete(Tools bytes2Sha anchorBlockId)
      if (theyConfirmedAlready) respondBecome(completeProto, commits, 'Normal)
      else respondStay(completeProto, wfc.modify(_.depthOk) setTo true)

    // They have confirmed an achor, maybe wait for us
    case (wfc @ WaitForConfirms(commits, false, depthOkAlready), pkt: proto.pkt,
      'OpenWaitOurAnchorConfirm | 'OpenWaitTheirAnchorConfirm) if has(pkt.open_complete) =>

      if (depthOkAlready) become(commits, 'Normal)
      else stayWith(wfc.modify(_.theyConfirmed) setTo true)

    // Uniclose a channel with their anchor
    case (_, 'Uniclose, 'OpenWaitTheirAnchorConfirm) =>
      authHandler process new proto.error(CHANNEL_CANCELLED)
      become(null, 'Closed)

    // Uniclose a channel with our anchor
    case (w: WaitForConfirms, 'Uniclose, 'OpenWaitOurAnchorConfirm) =>
      uniclose(w.commits.ourCommit, CHANNEL_CANCELLED)

    // They have uniclosed a channel with their anchor broadcasted
    case (_, pkt: proto.pkt, 'OpenWaitTheirAnchorConfirm) if has(pkt.error) =>
      become(null, 'Closed)

    // They have uniclosed a channel with our anchor broadcasted
    case (w: WaitForConfirms, pkt: proto.pkt, 'OpenWaitOurAnchorConfirm) if has(pkt.error) =>
      become(WaitForUniclose(w.commits.ourCommit), 'Uniclose)

    // MAIN LOOP

    // Send a brand new HTLC to them if we have enough funds
    case (c: Commitments, htlc: proto.update_add_htlc, 'Normal) if c.shutdown.isEmpty =>
      // Our available funds with pending changes as seen by them, pending incoming htlcs can't be spent
      val amount = c.theirCommit.spec.reduce(c.theirChanges.acked, c.ourChanges.proposed).amountThemMsat
      if (htlc.amount_msat > amount) throw new RuntimeException(NOT_ENOUGH_FUNDS)
      else respondStay(data = c addOurProposal toPkt(htlc), toThem = htlc)

    // Receive a brand new HTLC from them if they have enough funds
    case (c: Commitments, pkt: proto.pkt, 'Normal) if has(pkt.update_add_htlc) =>
      // Their available funds with pending changes as seen by us, pending incoming htlcs can't be spent
      val amount = c.ourCommit.spec.reduce(c.ourChanges.acked, c.theirChanges.proposed).amountThemMsat
      if (pkt.update_add_htlc.amount_msat > amount) uniclose(c.ourCommit, NOT_ENOUGH_FUNDS)
      else stayWith(c addTheirProposal pkt)

    // Send an HTLC fulfill for an HTLC they have sent to me before
    case (c: Commitments, fulfill: proto.update_fulfill_htlc, 'Normal) =>
      c.ourCommit.spec.htlcs collectFirst { case htlc if htlc.add.id == fulfill.id => htlc.add } match {
        case Some(add) if r2HashProto(fulfill.r) != add.r_hash => throw new RuntimeException(UNKNOWN_HTLC_PREIMAGE)
        case Some(add) => respondStay(data = c addOurProposal toPkt(fulfill), toThem = fulfill)
        case None => throw new RuntimeException(UNKNOWN_HTLC_ID)
      }

    // Receive a fulfill for an HTLC I've sent to them before
    case (c: Commitments, pkt: proto.pkt, 'Normal) if has(pkt.update_fulfill_htlc) =>
      c.theirCommit.spec.htlcs collectFirst { case htlc if htlc.add.id == pkt.update_fulfill_htlc.id => htlc.add } match {
        case Some(ourAddHtlc) if r2HashProto(pkt.update_fulfill_htlc.r) == ourAddHtlc.r_hash => stayWith(c addTheirProposal pkt)
        case Some(ourAddHtlc) => uniclose(c.ourCommit, UNKNOWN_HTLC_PREIMAGE)
        case None => uniclose(c.ourCommit, UNKNOWN_HTLC_ID)
      }

    // Send an HTLC fail for an HTLC they have sent to me before
    case (c: Commitments, fail: proto.update_fail_htlc, 'Normal) =>
      val theirHtlcFound = c.ourCommit.spec.htlcs.exists(_.add.id == fail.id)
      if (theirHtlcFound) respondStay(data = c addOurProposal toPkt(fail), toThem = fail)
      else throw new RuntimeException(UNKNOWN_HTLC_ID)

    // Receive a fail for an HTLC I've sent to them before
    case (c: Commitments, pkt: proto.pkt, 'Normal) if has(pkt.update_fail_htlc) =>
      val ourHtlcFound = c.theirCommit.spec.htlcs.exists(_.add.id == pkt.update_fail_htlc.id)
      if (ourHtlcFound) stayWith(c addTheirProposal pkt) else uniclose(c.ourCommit, UNKNOWN_HTLC_ID)

    // Send a commitment transaction to them
    case (c: Commitments, 'Sign, 'Normal) if c.weHaveChanges =>
      c.theirNextCommitInfo.right foreach { case theirNextRevocHash =>
        // Our vision of their commit now includes all our + their acked changes
        val spec1 = c.theirCommit.spec.reduce(c.theirChanges.acked, c.ourChanges.proposed)
        val theirCommitTxTemplate = c.makeTheirTxTemplate(Tools sha2Bytes theirNextRevocHash, spec1)
        val theirCommitTx = theirCommitTxTemplate.makeTx(c.ourCommit.publishableTx.getInputs.asScala:_*)
        val ourSig = Scripts.signTx(c.ourParams, c.theirParams, theirCommitTx, c.anchorOutput.getValue.value)
        // We do not actually sign a tranaction if *they* have no outputs and as such do not get paid
        val conditionalSignature = if (theirCommitTxTemplate.hasAnOutput) ourSig else null
        val ourSigProto = new proto.update_commit(conditionalSignature)

        // Our proposed changes are now signed
        // Their acked are included in our commit and removed
        // Save our vision of their next commit and await revocation
        val theirChanges1 = c.theirChanges.copy(acked = Vector.empty)
        val nextCommit = TheirCommit(c.theirCommit.index + 1, spec1, theirNextRevocHash)
        val ourChanges1 = c.ourChanges.copy(proposed = Vector.empty, signed = c.ourChanges.proposed)
        val c1 = c.copy(theirNextCommitInfo = Left(nextCommit), theirChanges = theirChanges1, ourChanges = ourChanges1)
        respondStay(data = c1, toThem = ourSigProto)
      }
  }

  // HELPERS

  def respondStay(toThem: Any, data: ChannelData) = {
    // Method for responding and staying in a current state
    authHandler process toThem
    stayWith(data)
  }

  def respondBecome(toThem: Any, data: ChannelData, state: Symbol) = {
    // Method for responding and transitioning into a new state if success
    authHandler process toThem
    become(data, state)
  }

  def uniclose(commit: OurCommit, reasonWhy: String) = {
    // Method for sending out an error and unilateral close
    authHandler process new proto.error(reasonWhy)
    become(WaitForUniclose(commit), 'Uniclose)
  }
}