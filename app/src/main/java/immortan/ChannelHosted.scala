package immortan

import fr.acinq.eclair._
import immortan.Channel._
import immortan.ErrorCodes._
import fr.acinq.eclair.wire._
import immortan.crypto.Tools._
import fr.acinq.eclair.channel._
import com.softwaremill.quicklens._
import fr.acinq.eclair.transactions._
import fr.acinq.bitcoin.{ByteVector64, SatoshiLong}
import fr.acinq.eclair.channel.Helpers.HashToPreimage
import fr.acinq.eclair.blockchain.CurrentBlockCount
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.payment.OutgoingPacket
import immortan.fsm.PreimageCheck
import scodec.bits.ByteVector


object ChannelHosted {
  def make(initListeners: Set[ChannelListener], hostedData: HostedCommits, bag: ChannelBag): ChannelHosted = new ChannelHosted {
    def SEND(msgs: LightningMessage*): Unit = CommsTower.sendMany(msgs.map(LightningMessageCodecs.prepareNormal), hostedData.remoteInfo.nodeSpecificPair)
    def STORE(hostedData: PersistentChannelData): PersistentChannelData = bag.put(hostedData)
    listeners = initListeners
    doProcess(hostedData)
  }
}

abstract class ChannelHosted extends Channel { me =>
  def isOutOfSync(blockDay: Long): Boolean = math.abs(blockDay - LNParams.currentBlockDay) > 1

  def doProcess(change: Any): Unit = Tuple3(data, change, state) match {
    case (wait: WaitRemoteHostedReply, CMD_SOCKET_ONLINE, WAIT_FOR_INIT) =>
      me SEND InvokeHostedChannel(LNParams.chainHash, wait.refundScriptPubKey, wait.secret)
      BECOME(wait, WAIT_FOR_ACCEPT)


    case (WaitRemoteHostedReply(remoteInfo, refundScriptPubKey, _), init: InitHostedChannel, WAIT_FOR_ACCEPT) =>
      if (init.initialClientBalanceMsat > init.channelCapacityMsat) throw new RuntimeException("Their init balance for us is larger than capacity")
      if (UInt64(100000000L) > init.maxHtlcValueInFlightMsat) throw new RuntimeException("Their max value in-flight is too low")
      if (init.htlcMinimumMsat > 546000L.msat) throw new RuntimeException("Their minimal payment size is too high")
      if (init.maxAcceptedHtlcs < 1) throw new RuntimeException("They can accept too few payments")

      val localHalfSignedHC =
        restoreCommits(LastCrossSignedState(isHost = false, refundScriptPubKey, init, LNParams.currentBlockDay, init.initialClientBalanceMsat,
          init.channelCapacityMsat - init.initialClientBalanceMsat, localUpdates = 0L, remoteUpdates = 0L, incomingHtlcs = Nil, outgoingHtlcs = Nil,
          localSigOfRemote = ByteVector64.Zeroes, remoteSigOfLocal = ByteVector64.Zeroes).withLocalSigOfRemote(remoteInfo.nodeSpecificPrivKey), remoteInfo)

      BECOME(WaitRemoteHostedStateUpdate(remoteInfo, localHalfSignedHC), WAIT_FOR_ACCEPT)
      SEND(localHalfSignedHC.lastCrossSignedState.stateUpdate)


    case (WaitRemoteHostedStateUpdate(_, localHalfSignedHC), remoteSU: StateUpdate, WAIT_FOR_ACCEPT) =>
      val localCompleteLCSS = localHalfSignedHC.lastCrossSignedState.copy(remoteSigOfLocal = remoteSU.localSigOfRemoteLCSS)
      val isRightRemoteUpdateNumber = localHalfSignedHC.lastCrossSignedState.remoteUpdates == remoteSU.localUpdates
      val isRightLocalUpdateNumber = localHalfSignedHC.lastCrossSignedState.localUpdates == remoteSU.remoteUpdates
      val isRemoteSigOk = localCompleteLCSS.verifyRemoteSig(localHalfSignedHC.remoteInfo.nodeId)
      val isBlockDayWrong = isOutOfSync(remoteSU.blockDay)

      if (isBlockDayWrong) throw new RuntimeException("Their blockday is wrong")
      if (!isRemoteSigOk) throw new RuntimeException("Their signature is wrong")
      if (!isRightRemoteUpdateNumber) throw new RuntimeException("Their remote update number is wrong")
      if (!isRightLocalUpdateNumber) throw new RuntimeException("Their local update number is wrong")
      StoreBecomeSend(localHalfSignedHC.copy(lastCrossSignedState = localCompleteLCSS), OPEN)


    case (wait: WaitRemoteHostedReply, remoteLCSS: LastCrossSignedState, WAIT_FOR_ACCEPT) =>
      // We have expected InitHostedChannel but got LastCrossSignedState so this channel exists already
      // make sure our signature match and if so then become OPEN using host supplied state data
      val isLocalSigOk = remoteLCSS.verifyRemoteSig(wait.remoteInfo.nodeSpecificPubKey)
      val isRemoteSigOk = remoteLCSS.reverse.verifyRemoteSig(wait.remoteInfo.nodeId)
      val hc = restoreCommits(remoteLCSS.reverse, wait.remoteInfo)

      if (!isRemoteSigOk) localSuspend(hc, ERR_HOSTED_WRONG_REMOTE_SIG)
      else if (!isLocalSigOk) localSuspend(hc, ERR_HOSTED_WRONG_LOCAL_SIG)
      else {
        StoreBecomeSend(hc, OPEN, hc.lastCrossSignedState)
        // Remote LCSS could contain pending incoming
        events.notifyResolvers
      }

    // CHANNEL IS ESTABLISHED

    case (hc: HostedCommits, CurrentBlockCount(tip), OPEN | SLEEPING) =>
      // Keep in mind that we may have many outgoing HTLCs which have the same preimage
      val sentExpired = hc.allOutgoing.filter(tip > _.cltvExpiry.toLong).groupBy(_.paymentHash)
      val hasReceivedRevealedExpired = hc.revealedFulfills.exists(tip > _.theirAdd.cltvExpiry.toLong)

      if (hasReceivedRevealedExpired) {
        // We have incoming payments for which we have revealed a preimage but they are still unresolved and completely expired
        // unless we have published a preimage on chain we can not prove we have revealed a preimage in time at this point
        // at the very least it makes sense to halt further usage of this potentially malicious channel
        localSuspend(hc, ERR_HOSTED_MANUAL_SUSPEND)
      }

      if (sentExpired.nonEmpty) {
        val checker = new PreimageCheck {
          override def onComplete(hash2preimage: HashToPreimage): Unit = {
            val settledOutgoingHtlcIds = sentExpired.values.flatten.map(_.id)
            val (fulfilled, failed) = sentExpired.values.flatten.partition(add => hash2preimage contains add.paymentHash)
            localSuspend(hc.modify(_.postErrorOutgoingResolvedIds).using(_ ++ settledOutgoingHtlcIds), ERR_HOSTED_TIMED_OUT_OUTGOING_HTLC)
            for (add <- fulfilled) events fulfillReceived RemoteFulfill(theirPreimage = hash2preimage(add.paymentHash), ourAdd = add)
            for (add <- failed) events addRejectedLocally InPrincipleNotSendable(localAdd = add)
            // There will be no state update
            events.notifyResolvers
          }
        }

        // Our peer might have published a preimage on chain instead of directly sending it to us
        // if it turns out that preimage is not present on chain at this point we can safely fail an HTLC
        checker process PreimageCheck.CMDStart(sentExpired.keySet, LNParams.syncParams.phcSyncNodes)
      }


    case (hc: HostedCommits, theirAdd: UpdateAddHtlc, OPEN) if hc.error.isEmpty =>
      val theirAddExt = UpdateAddHtlcExt(theirAdd, hc.remoteInfo)
      BECOME(hc.receiveAdd(theirAdd), OPEN)
      events addReceived theirAddExt


    case (hc: HostedCommits, msg: UpdateFulfillHtlc, OPEN | SLEEPING) if hc.error.isEmpty =>
      val remoteFulfill = hc.makeRemoteFulfill(msg)
      BECOME(hc.addRemoteProposal(msg), state)
      events fulfillReceived remoteFulfill


    case (hc: HostedCommits, msg: UpdateFulfillHtlc, OPEN | SLEEPING) if hc.error.isDefined =>
      // We may get into error state with this HTLC not expired yet so they may fulfill it afterwards
      val hc1 = hc.modify(_.postErrorOutgoingResolvedIds).using(_ + msg.id)
      // This will throw if HTLC has already been settled post-error
      val remoteFulfill = hc.makeRemoteFulfill(msg)
      BECOME(hc1.addRemoteProposal(msg), state)
      events fulfillReceived remoteFulfill
      // There will be no state update
      events.notifyResolvers


    case (hc: HostedCommits, msg: UpdateFailHtlc, OPEN) if hc.error.isEmpty => BECOME(hc.receiveFail(msg), OPEN)
    case (hc: HostedCommits, msg: UpdateFailMalformedHtlc, OPEN) if hc.error.isEmpty => BECOME(hc.receiveFailMalformed(msg), OPEN)


    case (hc: HostedCommits, CMD_SIGN, OPEN) if (hc.nextLocalUpdates.nonEmpty || hc.resizeProposal.isDefined) && hc.error.isEmpty =>
      val nextLocalLCSS = hc.resizeProposal.map(hc.withResize).getOrElse(hc).nextLocalUnsignedLCSS(LNParams.currentBlockDay)
      SEND(nextLocalLCSS.withLocalSigOfRemote(hc.remoteInfo.nodeSpecificPrivKey).stateUpdate)


    // First attempt a normal state update, then a resized state update if original signature check fails and we have a pending resize proposal
    case (hc: HostedCommits, remoteSU: StateUpdate, OPEN) if (remoteSU.localSigOfRemoteLCSS != hc.lastCrossSignedState.remoteSigOfLocal) && hc.error.isEmpty =>
      attemptStateUpdate(remoteSU, hc)


    case (hc: HostedCommits, cmd: CMD_ADD_HTLC, OPEN | SLEEPING) =>
      hc.sendAdd(cmd, blockHeight = LNParams.blockCount.get) match {
        case _ if hc.error.isDefined => events addRejectedLocally ChannelNotAbleToSend(cmd.incompleteAdd)
        case _ if SLEEPING == state => events addRejectedLocally ChannelOffline(cmd.incompleteAdd)
        case Left(reason) => events addRejectedLocally reason

        case Right(hc1 ~ updateAddHtlcMsg) =>
          StoreBecomeSend(hc1, OPEN, updateAddHtlcMsg)
          process(CMD_SIGN)
      }


    case (_, cmd: CMD_ADD_HTLC, _) =>
      // Instruct upstream to skip this channel in such a state
      val reason = ChannelNotAbleToSend(cmd.incompleteAdd)
      events addRejectedLocally reason


    // Fulfilling is allowed even in error state
    // CMD_SIGN will be sent from ChannelMaster strictly after outgoing FSM sends this command
    case (hc: HostedCommits, cmd: CMD_FULFILL_HTLC, OPEN) if hc.nextLocalSpec.findIncomingHtlcById(cmd.theirAdd.id).isDefined =>
      val msg = UpdateFulfillHtlc(hc.channelId, cmd.theirAdd.id, cmd.preimage)
      StoreBecomeSend(hc.addLocalProposal(msg), OPEN, msg)


    // CMD_SIGN will be sent from ChannelMaster strictly after outgoing FSM sends this command
    case (hc: HostedCommits, cmd: CMD_FAIL_HTLC, OPEN) if hc.nextLocalSpec.findIncomingHtlcById(cmd.theirAdd.id).isDefined && hc.error.isEmpty =>
      val msg = OutgoingPacket.buildHtlcFailure(cmd, theirAdd = cmd.theirAdd)
      StoreBecomeSend(hc.addLocalProposal(msg), OPEN, msg)


    // CMD_SIGN will be sent from ChannelMaster strictly after outgoing FSM sends this command
    case (hc: HostedCommits, cmd: CMD_FAIL_MALFORMED_HTLC, OPEN) if hc.nextLocalSpec.findIncomingHtlcById(cmd.theirAdd.id).isDefined && hc.error.isEmpty =>
      val msg = UpdateFailMalformedHtlc(hc.channelId, cmd.theirAdd.id, cmd.onionHash, cmd.failureCode)
      StoreBecomeSend(hc.addLocalProposal(msg), OPEN, msg)


    case (hc: HostedCommits, CMD_SOCKET_ONLINE, SLEEPING) =>
      val origRefundPubKey = hc.lastCrossSignedState.refundScriptPubKey
      val invokeMsg = InvokeHostedChannel(LNParams.chainHash, origRefundPubKey, ByteVector.empty)
      SEND(hc.error getOrElse invokeMsg)


    case (hc: HostedCommits, CMD_SOCKET_OFFLINE, OPEN) => BECOME(hc, SLEEPING)

    case (hc: HostedCommits, _: InitHostedChannel, SLEEPING) => SEND(hc.lastCrossSignedState)

    case (hc: HostedCommits, remoteLCSS: LastCrossSignedState, SLEEPING) if hc.error.isEmpty => attemptInitResync(hc, remoteLCSS)

    case (hc: HostedCommits, remoteInfo: RemoteNodeInfo, SLEEPING) if hc.remoteInfo.nodeId == remoteInfo.nodeId => StoreBecomeSend(hc.copy(remoteInfo = remoteInfo.safeAlias), SLEEPING)


    case (hc: HostedCommits, update: ChannelUpdate, OPEN | SLEEPING) if hc.updateOpt.forall(_.core != update.core) && hc.error.isEmpty =>
      val shortIdMatches = hostedShortChanId(hc.remoteInfo.nodeSpecificPubKey.value, hc.remoteInfo.nodeId.value) == update.shortChannelId
      if (shortIdMatches) StoreBecomeSend(hc.copy(updateOpt = update.asSome), state)


    case (hc: HostedCommits, cmd: HC_CMD_RESIZE, OPEN | SLEEPING) if hc.resizeProposal.isEmpty && hc.error.isEmpty =>
      val capacitySat = hc.lastCrossSignedState.initHostedChannel.channelCapacityMsat.truncateToSatoshi
      val resize = ResizeChannel(capacitySat + cmd.delta).sign(hc.remoteInfo.nodeSpecificPrivKey)
      StoreBecomeSend(hc.copy(resizeProposal = resize.asSome), state, resize)
      process(CMD_SIGN)


    case (hc: HostedCommits, resize: ResizeChannel, OPEN | SLEEPING) if hc.resizeProposal.isEmpty && hc.error.isEmpty =>
      // Can happen if we have sent a resize earlier, but then lost channel data and restored from their
      val isLocalSigOk: Boolean = resize.verifyClientSig(hc.remoteInfo.nodeSpecificPubKey)
      if (isLocalSigOk) StoreBecomeSend(hc.copy(resizeProposal = resize.asSome), state)
      else localSuspend(hc, ERR_HOSTED_INVALID_RESIZE)


    case (hc: HostedCommits, remoteSO: StateOverride, OPEN | SLEEPING) if hc.error.isDefined && !hc.overrideProposal.contains(remoteSO) =>
      StoreBecomeSend(hc.copy(overrideProposal = remoteSO.asSome), state)


    case (hc: HostedCommits, cmd @ CMD_HOSTED_STATE_OVERRIDE(remoteSO), OPEN | SLEEPING) if hc.error.isDefined =>
      val overriddenLocalBalance = hc.lastCrossSignedState.initHostedChannel.channelCapacityMsat - remoteSO.localBalanceMsat
      val completeLocalLCSS = hc.lastCrossSignedState.copy(incomingHtlcs = Nil, outgoingHtlcs = Nil, localBalanceMsat = overriddenLocalBalance,
        remoteBalanceMsat = remoteSO.localBalanceMsat, localUpdates = remoteSO.remoteUpdates, remoteUpdates = remoteSO.localUpdates, blockDay = remoteSO.blockDay,
        remoteSigOfLocal = remoteSO.localSigOfRemoteLCSS).withLocalSigOfRemote(hc.remoteInfo.nodeSpecificPrivKey)

      val isRemoteSigOk = completeLocalLCSS.verifyRemoteSig(hc.remoteInfo.nodeId)
      val hc1 = restoreCommits(completeLocalLCSS, hc.remoteInfo)

      if (completeLocalLCSS.localBalanceMsat < 0L.msat) throw CMDException("Override impossible: new local balance is larger than capacity", cmd)
      if (remoteSO.localUpdates < hc.lastCrossSignedState.remoteUpdates) throw CMDException("Override impossible: new local update number from remote host is wrong", cmd)
      if (remoteSO.remoteUpdates < hc.lastCrossSignedState.localUpdates) throw CMDException("Override impossible: new remote update number from remote host is wrong", cmd)
      if (remoteSO.blockDay < hc.lastCrossSignedState.blockDay) throw CMDException("Override impossible: new override blockday from remote host is not acceptable", cmd)
      if (!isRemoteSigOk) throw CMDException("Override impossible: new override signature from remote host is wrong", cmd)
      StoreBecomeSend(hc1, OPEN, completeLocalLCSS.stateUpdate)
      rejectOverriddenOutgoingAdds(hc, hc1)
      // We may have pendig incoming
      events.notifyResolvers


    case (hc: HostedCommits, remote: Fail, WAIT_FOR_ACCEPT | OPEN) if hc.remoteError.isEmpty =>
      StoreBecomeSend(data1 = hc.copy(remoteError = remote.asSome), OPEN)
      throw RemoteErrorException(ErrorExt extractDescription remote)


    case (_, remote: Fail, _) =>
      // Convert remote error to local exception, it will be dealt with upstream
      throw RemoteErrorException(ErrorExt extractDescription remote)


    case (null, wait: WaitRemoteHostedReply, -1) => super.become(wait, WAIT_FOR_INIT)
    case (null, hc: HostedCommits, -1) => super.become(hc, SLEEPING)
    case _ =>
  }

  def rejectOverriddenOutgoingAdds(hc: HostedCommits, hc1: HostedCommits): Unit =
    for (add <- hc.allOutgoing -- hc1.allOutgoing) events addRejectedLocally InPrincipleNotSendable(add)

  def restoreCommits(localLCSS: LastCrossSignedState, remoteInfo: RemoteNodeInfo): HostedCommits = {
    val inFlightHtlcs = localLCSS.incomingHtlcs.map(IncomingHtlc) ++ localLCSS.outgoingHtlcs.map(OutgoingHtlc)
    HostedCommits(remoteInfo.safeAlias, CommitmentSpec(feeratePerKw = FeeratePerKw(0L.sat), localLCSS.localBalanceMsat, localLCSS.remoteBalanceMsat, inFlightHtlcs.toSet),
      localLCSS, nextLocalUpdates = Nil, nextRemoteUpdates = Nil, updateOpt = None, postErrorOutgoingResolvedIds = Set.empty, localError = None, remoteError = None)
  }

  def localSuspend(hc: HostedCommits, errCode: String): Unit = {
    val localError = Fail(data = ByteVector.fromValidHex(errCode), channelId = hc.channelId)
    if (hc.localError.isEmpty) StoreBecomeSend(hc.copy(localError = localError.asSome), state, localError)
  }

  def attemptInitResync(hc: HostedCommits, remoteLCSS: LastCrossSignedState): Unit = {
    val hc1 = hc.resizeProposal.filter(_ isRemoteResized remoteLCSS).map(hc.withResize).getOrElse(hc) // They may have a resized LCSS
    val weAreEven = hc.lastCrossSignedState.remoteUpdates == remoteLCSS.localUpdates && hc.lastCrossSignedState.localUpdates == remoteLCSS.remoteUpdates
    val weAreAhead = hc.lastCrossSignedState.remoteUpdates > remoteLCSS.localUpdates || hc.lastCrossSignedState.localUpdates > remoteLCSS.remoteUpdates
    val isLocalSigOk = remoteLCSS.verifyRemoteSig(hc1.remoteInfo.nodeSpecificPubKey)
    val isRemoteSigOk = remoteLCSS.reverse.verifyRemoteSig(hc1.remoteInfo.nodeId)

    if (!isRemoteSigOk) localSuspend(hc1, ERR_HOSTED_WRONG_REMOTE_SIG)
    else if (!isLocalSigOk) localSuspend(hc1, ERR_HOSTED_WRONG_LOCAL_SIG)
    else if (weAreAhead || weAreEven) {
      SEND(List(hc.lastCrossSignedState) ++ hc1.resizeProposal ++ hc1.nextLocalUpdates:_*)
      // Forget about their unsigned updates, they are expected to resend
      BECOME(hc1.copy(nextRemoteUpdates = Nil), OPEN)
      events.notifyResolvers
    } else {
      val localUpdatesAcked = remoteLCSS.remoteUpdates - hc1.lastCrossSignedState.localUpdates
      val remoteUpdatesAcked = remoteLCSS.localUpdates - hc1.lastCrossSignedState.remoteUpdates

      val remoteUpdatesAccounted = hc1.nextRemoteUpdates take remoteUpdatesAcked.toInt
      val localUpdatesAccounted = hc1.nextLocalUpdates take localUpdatesAcked.toInt
      val localUpdatesLeftover = hc1.nextLocalUpdates drop localUpdatesAcked.toInt

      val hc2 = hc1.copy(nextLocalUpdates = localUpdatesAccounted, nextRemoteUpdates = remoteUpdatesAccounted)
      val syncedLCSS = hc2.nextLocalUnsignedLCSS(remoteLCSS.blockDay).copy(localSigOfRemote = remoteLCSS.remoteSigOfLocal, remoteSigOfLocal = remoteLCSS.localSigOfRemote)

      if (syncedLCSS.reverse == remoteLCSS) {
        // We have fallen behind a bit but have all the data required to successfully synchronize such that an updated state is reached
        val hc3 = hc2.copy(lastCrossSignedState = syncedLCSS, localSpec = hc2.nextLocalSpec, nextLocalUpdates = localUpdatesLeftover, nextRemoteUpdates = Nil)
        StoreBecomeSend(hc3, OPEN, List(syncedLCSS) ++ hc2.resizeProposal ++ localUpdatesLeftover:_*)
        events.notifyResolvers
      } else {
        // We are too far behind, restore from their future data
        val hc3 = restoreCommits(remoteLCSS.reverse, hc2.remoteInfo)
        StoreBecomeSend(hc3, OPEN, remoteLCSS.reverse)
        rejectOverriddenOutgoingAdds(hc1, hc3)
        events.notifyResolvers
      }
    }
  }

  def attemptStateUpdate(remoteSU: StateUpdate, hc: HostedCommits): Unit = {
    val lcss1 = hc.nextLocalUnsignedLCSS(remoteSU.blockDay).copy(remoteSigOfLocal = remoteSU.localSigOfRemoteLCSS).withLocalSigOfRemote(hc.remoteInfo.nodeSpecificPrivKey)
    val hc1 = hc.copy(lastCrossSignedState = lcss1, localSpec = hc.nextLocalSpec, nextLocalUpdates = Nil, nextRemoteUpdates = Nil)
    val isRemoteSigOk = lcss1.verifyRemoteSig(hc.remoteInfo.nodeId)
    val isBlockDayWrong = isOutOfSync(remoteSU.blockDay)

    if (isBlockDayWrong) {
      // We could suspend, but instead choose to wait for chain wallet to synchronize
      CommsTower.workers.get(hc.remoteInfo.nodeSpecificPair).foreach(_.disconnect)
    } else if (remoteSU.remoteUpdates < lcss1.localUpdates) {
      // Persist unsigned remote updates to use them on re-sync
      // we do not update runtime data because ours is newer one
      process(CMD_SIGN)
      me STORE hc
    } else if (!isRemoteSigOk) {
      hc.resizeProposal.map(hc.withResize) match {
        case Some(resizedHC) => attemptStateUpdate(remoteSU, resizedHC)
        case None => localSuspend(hc, ERR_HOSTED_WRONG_REMOTE_SIG)
      }
    } else {
      val remoteRejects: Seq[RemoteReject] = hc.nextRemoteUpdates.collect {
        case fail: UpdateFailHtlc => RemoteUpdateFail(fail, hc.localSpec.findOutgoingHtlcById(fail.id).get.add)
        case malform: UpdateFailMalformedHtlc => RemoteUpdateMalform(malform, hc.localSpec.findOutgoingHtlcById(malform.id).get.add)
      }

      StoreBecomeSend(hc1, OPEN, lcss1.stateUpdate)
      for (reject <- remoteRejects) events addRejectedRemotely reject
      events.notifyResolvers
    }
  }
}
