package immortan.utils

import fr.acinq.eclair._
import immortan.utils.InputParser._
import scala.util.{Failure, Success, Try}
import immortan.{LNParams, RemoteNodeInfo}
import fr.acinq.eclair.payment.PaymentRequest
import scala.util.matching.{Regex, UnanchoredRegex}
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.eclair.wire.NodeAddress
import immortan.crypto.Tools.trimmed
import fr.acinq.bitcoin.ScriptElt
import scodec.bits.ByteVector
import immortan.utils.uri.Uri


object InputParser {
  var value: Any = new String
  case object DoNotEraseRecordedValue
  type Checker = PartialFunction[Any, Any]

  def checkAndMaybeErase(fun: Checker): Unit = fun(value) match {
    case DoNotEraseRecordedValue => // Do nothing, value is retained
    case _ => value = null // Erase recorded value
  }

  private[this] val prefixes = PaymentRequest.prefixes.values mkString "|"

  private[this] val lnUrl = "(?im).*?(lnurl)([0-9]+[a-z0-9]+)".r.unanchored

  private[this] val shortNodeLink = "([a-fA-F0-9]{66})@([a-zA-Z0-9:.\\-_]+)".r.unanchored

  val nodeLink: UnanchoredRegex = "([a-fA-F0-9]{66})@([a-zA-Z0-9:.\\-_]+):([0-9]+)".r.unanchored

  val lnPayReq: UnanchoredRegex = s"(?im).*?($prefixes)([0-9]{1,}[a-z0-9]+){1}".r.unanchored

  val identifier: UnanchoredRegex = "^[a-zA-Z0-9_.]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$".r.unanchored

  val lightning: String = "lightning:"

  val bitcoin: String = "bitcoin:"

  def recordValue(raw: String): Unit = value = parse(raw)

  def parse(rawInput: String): Any = rawInput take 2880 match {
    case lnUrl(prefix, data) => LNUrl.fromBech32(s"$prefix$data")
    case nodeLink(key, host, port) => RemoteNodeInfo(PublicKey.fromBin(ByteVector fromValidHex key), NodeAddress.fromParts(host, port.toInt), host)
    case shortNodeLink(key, host) => RemoteNodeInfo(PublicKey.fromBin(ByteVector fromValidHex key), NodeAddress.fromParts(host, port = 9735), host)
    case lnPayReq(prefix, data) => PaymentRequestExt.fromRaw(s"$prefix$data")

    case _ =>
      val isLightning = rawInput.toLowerCase.startsWith(lightning)
      val withoutPrefix = PaymentRequestExt.removePrefix(rawInput).trim
      val isLightningAddress = identifier.findFirstMatchIn(withoutPrefix)
      if (isLightningAddress.isDefined) LNUrl.fromIdentifier(withoutPrefix)
      else if (isLightning) PaymentRequestExt.fromUri(withoutPrefix.toLowerCase)
      else BitcoinUri.fromRaw(s"$bitcoin$withoutPrefix")
  }
}

object PaymentRequestExt {
  def removePrefix(raw: String): String = raw.split(':').toList match {
    case prefix :: content if lightning.startsWith(prefix.toLowerCase) => content.mkString.replace("//", "")
    case prefix :: content if bitcoin.startsWith(prefix.toLowerCase) => content.mkString.replace("//", "")
    case _ => raw
  }

  def fromUri(invoiceWithoutPrefix: String): PaymentRequestExt = {
    val lnPayReq(invoicePrefix, invoiceData) = invoiceWithoutPrefix
    val uri = Try(Uri parse s"$lightning//$invoiceWithoutPrefix")
    val pr = PaymentRequest.read(s"$invoicePrefix$invoiceData")
    PaymentRequestExt(uri, pr, s"$invoicePrefix$invoiceData")
  }

  def fromRaw(raw: String): PaymentRequestExt = {
    val noUri: Try[Uri] = Failure(new RuntimeException)
    PaymentRequestExt(noUri, PaymentRequest.read(raw), raw)
  }

  def from(pr: PaymentRequest): PaymentRequestExt = {
    val noUri: Try[Uri] = Failure(new RuntimeException)
    PaymentRequestExt(noUri, pr, PaymentRequest write pr)
  }
}

case class PaymentRequestExt(uri: Try[Uri], pr: PaymentRequest, raw: String) {
  def withNewSplit(anotherPart: MilliSatoshi): String = s"$lightning$raw?splits=" + (anotherPart :: splits).map(_.toLong).mkString(",")
  val splits: List[MilliSatoshi] = uri.map(_.getQueryParameter("splits").split(',').toList.map(_.toLong) map MilliSatoshi.apply).getOrElse(Nil)
  val hasSplitIssue: Boolean = pr.amount.exists(splits.sum + LNParams.minPayment > _) || (pr.amount.isEmpty && splits.nonEmpty)
  val splitLeftover: MilliSatoshi = pr.amount.map(_ - splits.sum).getOrElse(0L.msat)

  val descriptionOpt: Option[String] = pr.description.left.toOption.map(trimmed).filter(_.nonEmpty)
  val brDescription: String = descriptionOpt.map(desc => s"<br><br>$desc").getOrElse(new String)
  val descriptionOrEmpty: String = descriptionOpt.getOrElse(new String)
}

object BitcoinUri {
  def fromRaw(raw: String): BitcoinUri = {
    val dataWithoutPrefix = PaymentRequestExt.removePrefix(raw)
    val uri = Uri.parse(s"$bitcoin//$dataWithoutPrefix")
    BitcoinUri(Success(uri), uri.getHost)
  }
}

case class BitcoinUri(uri: Try[Uri], address: String) {
  val isValid: Boolean = Try(pubKeyScript).toOption.exists(_.nonEmpty)
  val amount: Option[MilliSatoshi] = uri.map(_ getQueryParameter "amount").map(BigDecimal.apply).map(Denomination.btcBigDecimal2MSat).toOption
  val prExt: Option[PaymentRequestExt] = uri.map(_ getQueryParameter "lightning").map(PaymentRequestExt.fromRaw).toOption
  val message: Option[String] = uri.map(_ getQueryParameter "message").map(trimmed).filter(_.nonEmpty).toOption
  val label: Option[String] = uri.map(_ getQueryParameter "label").map(trimmed).filter(_.nonEmpty).toOption
  def pubKeyScript: Seq[ScriptElt] = addressToPublicKeyScript(address, LNParams.chainHash)
}
