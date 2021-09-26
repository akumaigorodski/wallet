package immortan

import immortan.crypto.Tools._
import scala.concurrent.duration._
import akka.actor.{Actor, OneForOneStrategy, Props, SupervisorStrategy}


class LoggingSupervisor(childProps: Props, childName: String) extends Actor {

  import immortan.LNParams.logBag

  private val child = context.actorOf(childProps, childName)

  override def receive: Receive = { case anything => child forward anything }

  override val supervisorStrategy: OneForOneStrategy = OneForOneStrategy(loggingEnabled = false, maxNrOfRetries = 100, withinTimeRange = 1.minute) {
    // We allow at most <maxNrOfRetries> within <withinTimeRange>, otherwise the child actor is not restarted (this avoids child restart loops)

    case childError =>
      // Persist child failure and provide a strategy
      logBag.put(childName, childError.stackTraceAsString)
      SupervisorStrategy.Resume
  }
}
