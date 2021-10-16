package immortan.fsm

import fr.acinq.eclair.payment.PaymentRequest
import fr.acinq.eclair.wire._
import immortan.crypto.Tools.runAnd
import immortan.utils.Rx
import immortan.{ChanAndCommits, CommsTower, ConnectionListener}
import rx.lang.scala.Subscription

import scala.concurrent.duration._


abstract class SwapInHandler(cnc: ChanAndCommits, paymentRequest: PaymentRequest, id: Long) { me =>
  val shutdownTimer: Subscription = Rx.ioQueue.delay(30.seconds).doOnCompleted(finish).subscribe(_ => onTimeout)
  CommsTower.listenNative(Set(swapInListener), cnc.commits.remoteInfo)

  def finish: Unit = {
    // It is assumed that this FSM is established with a peer which has an HC with us
    // This FSM has a hardcoded timeout which will eventually remove its connection listener
    // OTOH this FSM should survive reconnects so there is no local disconnect logic here
    CommsTower.rmListenerNative(cnc.commits.remoteInfo, swapInListener)
    shutdownTimer.unsubscribe
  }

  lazy private val swapInListener = new ConnectionListener {
    override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = {
      val swapInRequest = SwapInPaymentRequest(PaymentRequest.write(paymentRequest), id)
      worker.handler process swapInRequest
    }

    override def onSwapInMessage(worker: CommsTower.Worker, msg: SwapIn): Unit = msg match {
      case message: SwapInState if message.processing.exists(_.id == id) => runAnd(finish)(onProcessing)
      case message: SwapInPaymentDenied if message.id == id => runAnd(finish)(me onDenied message)
      case _ => // Do nothing, it's unrelated
    }
  }

  def onDenied(msg: SwapInPaymentDenied): Unit
  def onProcessing: Unit
  def onTimeout: Unit
}
