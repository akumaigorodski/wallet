package immortan

import com.softwaremill.quicklens._
import fr.acinq.bitcoin.{ByteVector32, ByteVector64}
import fr.acinq.eclair._
import fr.acinq.eclair.channel._
import fr.acinq.eclair.transactions._
import fr.acinq.eclair.wire._
import immortan.crypto.Tools.{Any2Some, hostedChanId}
import scodec.bits.ByteVector


case class WaitRemoteHostedReply(remoteInfo: RemoteNodeInfo, refundScriptPubKey: ByteVector, secret: ByteVector) extends ChannelData

case class WaitRemoteHostedStateUpdate(remoteInfo: RemoteNodeInfo, hc: HostedCommits) extends ChannelData

case class HostedCommits(remoteInfo: RemoteNodeInfo, localSpec: CommitmentSpec, lastCrossSignedState: LastCrossSignedState,
                         nextLocalUpdates: List[UpdateMessage], nextRemoteUpdates: List[UpdateMessage], updateOpt: Option[ChannelUpdate], postErrorOutgoingResolvedIds: Set[Long],
                         localError: Option[Fail], remoteError: Option[Fail], resizeProposal: Option[ResizeChannel] = None, overrideProposal: Option[StateOverride] = None,
                         extParams: List[ExtParams] = Nil, startedAt: Long = System.currentTimeMillis) extends PersistentChannelData with Commitments { me =>

  lazy val error: Option[Fail] = localError.orElse(remoteError)

  lazy val nextTotalLocal: Long = lastCrossSignedState.localUpdates + nextLocalUpdates.size

  lazy val nextTotalRemote: Long = lastCrossSignedState.remoteUpdates + nextRemoteUpdates.size

  lazy val nextLocalSpec: CommitmentSpec = CommitmentSpec.reduce(localSpec, nextLocalUpdates, nextRemoteUpdates)

  lazy val channelId: ByteVector32 = hostedChanId(remoteInfo.nodeSpecificPubKey.value, remoteInfo.nodeId.value)

  lazy val allOutgoing: Set[UpdateAddHtlc] = {
    val allOutgoingAdds = localSpec.outgoingAdds ++ nextLocalSpec.outgoingAdds
    allOutgoingAdds.filterNot(add => postErrorOutgoingResolvedIds contains add.id)
  }

  lazy val crossSignedIncoming: Set[UpdateAddHtlcExt] = for (theirAdd <- localSpec.incomingAdds) yield UpdateAddHtlcExt(theirAdd, remoteInfo)

  lazy val revealedFulfills: Set[LocalFulfill] = getPendingFulfills(Helpers extractRevealedPreimages nextLocalUpdates)

  lazy val maxSendInFlight: MilliSatoshi = lastCrossSignedState.initHostedChannel.maxHtlcValueInFlightMsat.toMilliSatoshi

  lazy val minSendable: MilliSatoshi = lastCrossSignedState.initHostedChannel.htlcMinimumMsat

  lazy val availableForReceive: MilliSatoshi = nextLocalSpec.toRemote

  lazy val availableForSend: MilliSatoshi = nextLocalSpec.toLocal

  override def ourBalance: MilliSatoshi = availableForSend

  def nextLocalUnsignedLCSS(blockDay: Long): LastCrossSignedState =
    LastCrossSignedState(lastCrossSignedState.isHost, lastCrossSignedState.refundScriptPubKey, lastCrossSignedState.initHostedChannel,
      blockDay = blockDay, localBalanceMsat = nextLocalSpec.toLocal, remoteBalanceMsat = nextLocalSpec.toRemote, nextTotalLocal, nextTotalRemote,
      nextLocalSpec.incomingAdds.toList.sortBy(_.id), nextLocalSpec.outgoingAdds.toList.sortBy(_.id), localSigOfRemote = ByteVector64.Zeroes,
      remoteSigOfLocal = ByteVector64.Zeroes)

  def addLocalProposal(update: UpdateMessage): HostedCommits = copy(nextLocalUpdates = nextLocalUpdates :+ update)
  def addRemoteProposal(update: UpdateMessage): HostedCommits = copy(nextRemoteUpdates = nextRemoteUpdates :+ update)

  type UpdatedHCAndAdd = (HostedCommits, UpdateAddHtlc)
  def sendAdd(cmd: CMD_ADD_HTLC, blockHeight: Long): Either[LocalReject, UpdatedHCAndAdd] = {
    val completeAdd = cmd.incompleteAdd.copy(channelId = channelId, id = nextTotalLocal + 1)
    val commits1 = addLocalProposal(completeAdd)

    if (cmd.payload.amount < minSendable) return ChannelNotAbleToSend(cmd.incompleteAdd).asLeft
    if (CltvExpiry(blockHeight) >= cmd.cltvExpiry) return InPrincipleNotSendable(cmd.incompleteAdd).asLeft
    if (LNParams.maxCltvExpiryDelta.toCltvExpiry(blockHeight) < cmd.cltvExpiry) return InPrincipleNotSendable(cmd.incompleteAdd).asLeft
    if (commits1.nextLocalSpec.outgoingAdds.size > lastCrossSignedState.initHostedChannel.maxAcceptedHtlcs) return ChannelNotAbleToSend(cmd.incompleteAdd).asLeft
    if (commits1.allOutgoing.foldLeft(0L.msat)(_ + _.amountMsat) > maxSendInFlight) return ChannelNotAbleToSend(cmd.incompleteAdd).asLeft
    if (commits1.nextLocalSpec.toLocal < 0L.msat) return ChannelNotAbleToSend(cmd.incompleteAdd).asLeft
    Right(commits1, completeAdd)
  }

  def receiveAdd(add: UpdateAddHtlc): HostedCommits = {
    val commits1: HostedCommits = addRemoteProposal(add)
    // We do not check whether total incoming amount exceeds maxHtlcValueInFlightMsat becase we always accept up to channel capacity
    if (commits1.nextLocalSpec.incomingAdds.size > lastCrossSignedState.initHostedChannel.maxAcceptedHtlcs) throw ChannelTransitionFail(channelId)
    if (commits1.nextLocalSpec.toRemote < 0L.msat) throw ChannelTransitionFail(channelId)
    if (add.id != nextTotalRemote + 1) throw ChannelTransitionFail(channelId)
    commits1
  }

  // Relaxed constraints for receiveng preimages over HCs: we look at nextLocalSpec, not localSpec
  def makeRemoteFulfill(fulfill: UpdateFulfillHtlc): RemoteFulfill = nextLocalSpec.findOutgoingHtlcById(fulfill.id) match {
    case Some(ourAdd) if ourAdd.add.paymentHash != fulfill.paymentHash => throw ChannelTransitionFail(channelId)
    case _ if postErrorOutgoingResolvedIds.contains(fulfill.id) => throw ChannelTransitionFail(channelId)
    case Some(ourAdd) => RemoteFulfill(ourAdd.add, fulfill.paymentPreimage)
    case None => throw ChannelTransitionFail(channelId)
  }

  def withResize(resize: ResizeChannel): HostedCommits =
    me.modify(_.lastCrossSignedState.initHostedChannel.maxHtlcValueInFlightMsat).setTo(resize.newCapacityMsatU64)
      .modify(_.lastCrossSignedState.initHostedChannel.channelCapacityMsat).setTo(resize.newCapacity.toMilliSatoshi)
      .modify(_.localSpec.toRemote).using(_ + resize.newCapacity - lastCrossSignedState.initHostedChannel.channelCapacityMsat)
      .modify(_.resizeProposal).setTo(None)
}