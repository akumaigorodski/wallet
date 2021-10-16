package immortan.fsm

import fr.acinq.bitcoin.{ByteVector32, Satoshi}
import fr.acinq.eclair.wire._
import immortan.crypto.Tools._
import immortan.utils.Rx
import immortan.{ChanAndCommits, CommsTower, ConnectionListener}
import rx.lang.scala.Subscription

import scala.concurrent.duration._


abstract class SwapOutHandler(cnc: ChanAndCommits, amount: Satoshi, btcAddress: String, blockTarget: Int, feerateKey: ByteVector32) { me =>
  val shutdownTimer: Subscription = Rx.ioQueue.delay(30.seconds).doOnCompleted(finish).subscribe(_ => onTimeout)
  CommsTower.listenNative(Set(swapOutListener), cnc.commits.remoteInfo)

  def finish: Unit = {
    // It is assumed that this FSM is established with a peer which has an HC with us
    // This FSM has a hardcoded timeout which will eventually remove its connection listener
    // OTOH this FSM should survive reconnects so there is no local disconnect logic here
    CommsTower.rmListenerNative(cnc.commits.remoteInfo, swapOutListener)
    shutdownTimer.unsubscribe
  }

  lazy private val swapOutListener = new ConnectionListener {
    override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = {
      val swapOutRequest = SwapOutTransactionRequest(amount, btcAddress, blockTarget, feerateKey)
      worker.handler process swapOutRequest
    }

    override def onSwapOutMessage(worker: CommsTower.Worker, msg: SwapOut): Unit = msg match {
      case SwapOutTransactionDenied(btcAddr, SwapOutTransactionDenied.UNKNOWN_CHAIN_FEERATES) if btcAddr == btcAddress => runAnd(finish)(onPeerCanNotHandle)
      case SwapOutTransactionDenied(btcAddr, SwapOutTransactionDenied.CAN_NOT_HANDLE_AMOUNT) if btcAddr == btcAddress => runAnd(finish)(onPeerCanNotHandle)
      case message: SwapOutTransactionResponse if message.btcAddress == btcAddress => runAnd(finish)(me onResponse message)
      case message: SwapOutTransactionDenied if message.btcAddress == btcAddress => runAnd(finish)(onInvalidRequest)
      case _ => // Do nothing, it's unrelated
    }
  }

  def onResponse(message: SwapOutTransactionResponse): Unit
  def onPeerCanNotHandle: Unit
  def onInvalidRequest: Unit
  def onTimeout: Unit
}
