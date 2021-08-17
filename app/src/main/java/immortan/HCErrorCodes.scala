package immortan

import fr.acinq.eclair.wire.Fail


object ErrorCodes {
  final val ERR_HOSTED_WRONG_BLOCKDAY = "0001"
  final val ERR_HOSTED_WRONG_LOCAL_SIG = "0002"
  final val ERR_HOSTED_WRONG_REMOTE_SIG = "0003"
  final val ERR_HOSTED_CLOSED_BY_REMOTE_PEER = "0004"
  final val ERR_HOSTED_TIMED_OUT_OUTGOING_HTLC = "0005"
  final val ERR_HOSTED_HTLC_EXTERNAL_FULFILL = "0006"
  final val ERR_HOSTED_CHANNEL_DENIED = "0007"
  final val ERR_HOSTED_MANUAL_SUSPEND = "0008"
  final val ERR_HOSTED_INVALID_RESIZE = "0009"
  final val ERR_MISSING_CHANNEL = "0010"

  val knownHostedCodes: Map[String, String] = Map (
    ERR_HOSTED_WRONG_BLOCKDAY -> "ERR_HOSTED_WRONG_BLOCKDAY",
    ERR_HOSTED_WRONG_LOCAL_SIG -> "ERR_HOSTED_WRONG_LOCAL_SIG",
    ERR_HOSTED_WRONG_REMOTE_SIG -> "ERR_HOSTED_WRONG_REMOTE_SIG",
    ERR_HOSTED_CLOSED_BY_REMOTE_PEER -> "ERR_HOSTED_CLOSED_BY_REMOTE_PEER",
    ERR_HOSTED_TIMED_OUT_OUTGOING_HTLC -> "ERR_HOSTED_TIMED_OUT_OUTGOING_HTLC",
    ERR_HOSTED_HTLC_EXTERNAL_FULFILL -> "ERR_HOSTED_HTLC_EXTERNAL_FULFILL",
    ERR_HOSTED_CHANNEL_DENIED -> "ERR_HOSTED_CHANNEL_DENIED",
    ERR_HOSTED_MANUAL_SUSPEND -> "ERR_HOSTED_MANUAL_SUSPEND",
    ERR_HOSTED_INVALID_RESIZE -> "ERR_HOSTED_INVALID_RESIZE",
    ERR_MISSING_CHANNEL -> "ERR_MISSING_CHANNEL"
  )
}

object ErrorExt {
  def extractDescription(error: Fail): String = {
    val postTagData = error.data.drop(4)
    val tag = error.toAscii.take(4)

    ErrorCodes.knownHostedCodes.get(tag) match {
      case Some(codeOnly) if postTagData.isEmpty => s"hosted-code=$codeOnly"
      case Some(code) => s"hosted-code=$code, extra=${error.copy(data = postTagData).toAscii}"
      case None => error.toAscii
    }
  }
}
