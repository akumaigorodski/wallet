package immortan.utils

import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._
import fr.acinq.eclair.blockchain.CurrentBlockCount
import immortan.crypto.Tools.none
import akka.actor.Actor


object WalletEventsCatcher {
  case class Remove(listener: WalletEventsListener)
}

class WalletEventsCatcher extends Actor {
  // Not using a set to ensure insertion order
  var listeners: List[WalletEventsListener] = Nil

  context.system.eventStream.subscribe(channel = classOf[WalletEvent], subscriber = self)
  context.system.eventStream.subscribe(channel = classOf[CurrentBlockCount], subscriber = self)

  override def receive: Receive = {
    case listener: WalletEventsListener => listeners = (listeners :+ listener).distinct
    case WalletEventsCatcher.Remove(listener) => listeners = listeners diff List(listener)

    case event: WalletReady => for (lst <- listeners) lst.onChainSynchronized(event)
    case event: TransactionReceived => for (lst <- listeners) lst.onTransactionReceived(event)
    case event: CurrentBlockCount => for (lst <- listeners) lst.onChainTipKnown(event)
  }
}

class WalletEventsListener {
  def onChainSynchronized(event: WalletReady): Unit = none
  def onTransactionReceived(event: TransactionReceived): Unit = none
  def onChainTipKnown(event: CurrentBlockCount): Unit = none
}
