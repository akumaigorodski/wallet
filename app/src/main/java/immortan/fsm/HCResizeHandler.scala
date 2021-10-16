package immortan.fsm

import fr.acinq.bitcoin.Satoshi
import fr.acinq.eclair.channel.HC_CMD_RESIZE
import immortan.ChannelListener.Transition
import immortan.{ChannelHosted, ChannelListener, HostedCommits}


// Successful resize may come from a different handler, client should always re-check if new capacity is OK
abstract class HCResizeHandler(delta: Satoshi, chan: ChannelHosted) extends ChannelListener { me =>
  def onResizingSuccessful(hc1: HostedCommits): Unit
  def onChannelSuspended(hc1: HostedCommits): Unit

  override def onBecome: PartialFunction[Transition, Unit] = {
    case (_, prevHc: HostedCommits, nextHc: HostedCommits, _, _)
      if prevHc.error.isEmpty && nextHc.error.nonEmpty =>
      onChannelSuspended(nextHc)
      chan.listeners -= me

    case(_, prevHc: HostedCommits, nextHc: HostedCommits, _, _)
      if prevHc.resizeProposal.isDefined && nextHc.resizeProposal.isEmpty =>
      onResizingSuccessful(nextHc)
      chan.listeners -= me
  }

  chan.listeners += me
  chan process HC_CMD_RESIZE(delta)
}
