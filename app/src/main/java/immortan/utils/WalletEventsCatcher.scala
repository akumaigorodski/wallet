package immortan.utils

import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._
import fr.acinq.eclair.blockchain.electrum.ElectrumClient._
import fr.acinq.eclair.blockchain.electrum.ElectrumChainSync
import fr.acinq.eclair.blockchain.CurrentBlockCount
import java.net.InetSocketAddress
import immortan.crypto.Tools.none
import akka.actor.Actor


object WalletEventsCatcher {
  case class Remove(listener: WalletEventsListener)
}

class WalletEventsCatcher extends Actor {
  // Not using a set to ensure insertion order
  var listeners: List[WalletEventsListener] = Nil

  context.system.eventStream.subscribe(channel = classOf[WalletEvent], subscriber = self)
  context.system.eventStream.subscribe(channel = classOf[ElectrumEvent], subscriber = self)
  context.system.eventStream.subscribe(channel = classOf[CurrentBlockCount], subscriber = self)
  context.system.eventStream.subscribe(channel = classOf[ElectrumChainSync.ChainSyncStarted], subscriber = self)
  context.system.eventStream.subscribe(channel = classOf[ElectrumChainSync.ChainSyncEnded], subscriber = self)

  override def receive: Receive = {
    case listener: WalletEventsListener => listeners = (listeners :+ listener).distinct
    case WalletEventsCatcher.Remove(listener) => listeners = listeners diff List(listener)

    case event: WalletReady => for (lst <- listeners) lst.onWalletReady(event)
    case event: TransactionReceived => for (lst <- listeners) lst.onTransactionReceived(event)

    case event: CurrentBlockCount => for (lst <- listeners) lst.onChainTipKnown(event)
    case event: ElectrumReady => for (lst <- listeners) lst.onChainMasterSelected(event.serverAddress)
    case ElectrumDisconnected => for (lst <- listeners) lst.onChainDisconnected

    case event: ElectrumChainSync.ChainSyncStarted => for (lst <- listeners) lst.onChainSyncStarted(event.localTip, event.remoteTip)
    case event: ElectrumChainSync.ChainSyncEnded => for (lst <- listeners) lst.onChainSyncEnded(event.localTip)
  }
}

class WalletEventsListener {
  def onWalletReady(event: WalletReady): Unit = none
  def onTransactionReceived(event: TransactionReceived): Unit = none

  def onChainTipKnown(event: CurrentBlockCount): Unit = none
  def onChainMasterSelected(event: InetSocketAddress): Unit = none
  def onChainDisconnected: Unit = none

  def onChainSyncStarted(localTip: Long, remoteTip: Long): Unit = none
  def onChainSyncEnded(localTip: Long): Unit = none
}
