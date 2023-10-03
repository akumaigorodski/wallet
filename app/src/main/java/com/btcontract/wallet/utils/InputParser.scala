package com.btcontract.wallet.utils

import com.btcontract.wallet.utils.InputParser._
import fr.acinq.bitcoin.{BtcAmount, Satoshi, SatoshiLong}
import fr.acinq.eclair._
import immortan.crypto.Tools.trimmed
import immortan.utils.Denomination
import immortan.utils.uri.Uri

import scala.util.parsing.combinator.RegexParsers
import scala.util.{Success, Try}


object InputParser {
  var value: Any = new String
  case object DoNotEraseRecordedValue
  type Checker = PartialFunction[Any, Any]

  def checkAndMaybeErase(fun: Checker): Unit = fun(value) match {
    case DoNotEraseRecordedValue => // Do nothing, value is retained
    case _ => value = null // Erase recorded value
  }

  def removePrefix(raw: String): String = raw.split(':').toList match {
    case prefix :: content if bitcoin.startsWith(prefix.toLowerCase) =>
      content.mkString.replace("//", "")
    case _ => raw
  }

  def withoutSlashes(prefix: String, uri: Uri): String = prefix + removePrefix(uri.toString)

  def recordValue(raw: String): Unit = value = parse(raw)

  private[this] val lnUrl = "(?im).*?(lnurl)([0-9]+[a-z0-9]+)".r.unanchored

  val bitcoin: String = "bitcoin:"

  def parse(rawInput: String): Any = rawInput take 2880 match {
    case lnUrl(prefix, data) => LNUrl.fromBech32(s"$prefix$data")

    case _ =>
      val withoutSlashes = removePrefix(rawInput).trim
      val addressToAmount = MultiAddressParser.parseAll(MultiAddressParser.parse, rawInput)
      addressToAmount getOrElse BitcoinUri.fromRaw(s"$bitcoin$withoutSlashes")
  }
}

object BitcoinUri {
  def fromRaw(raw: String): BitcoinUri = {
    val dataWithoutPrefix = InputParser.removePrefix(raw)
    val uri = Uri.parse(s"$bitcoin//$dataWithoutPrefix")
    BitcoinUri(Success(uri), uri.getHost)
  }
}

case class BitcoinUri(uri: Try[Uri], address: String) {
  val amount: Option[MilliSatoshi] = uri.map(_ getQueryParameter "amount").map(BigDecimal.apply).map(Denomination.btcBigDecimal2MSat).toOption
  val message: Option[String] = uri.map(_ getQueryParameter "message").map(trimmed).filter(_.nonEmpty).toOption
  val label: Option[String] = uri.map(_ getQueryParameter "label").map(trimmed).filter(_.nonEmpty).toOption
}

object MultiAddressParser extends RegexParsers {

  type AddressToAmountItem = (String, Satoshi)

  case class AddressToAmount(values: Seq[AddressToAmountItem] = Nil)

  private[this] val longSat = "[0-9,]+".r ^^ (_.replace(",", "").toLong.sat)

  private[this] val decimalSat = "[0-9]*\\.[0-9]+".r ^^ (raw => (BigDecimal(raw) * BtcAmount.Coin).toLong.sat)

  private[this] val item = "\\w+".r ~ (decimalSat | longSat) ^^ { case address ~ sat => address -> sat }

  private[this] val separator = opt(";")

  val parse: Parser[AddressToAmount] = repsep(item, separator).map(AddressToAmount)
}
