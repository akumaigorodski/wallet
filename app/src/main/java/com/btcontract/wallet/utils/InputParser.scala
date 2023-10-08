package com.btcontract.wallet.utils

import com.btcontract.wallet.utils.InputParser._
import com.sparrowwallet.drongo.crypto.Bip322
import fr.acinq.bitcoin.{BtcAmount, ByteVector32, Satoshi, SatoshiLong}
import fr.acinq.eclair._
import immortan.crypto.Tools._
import immortan.utils.Denomination
import immortan.utils.uri.Uri
import org.bouncycastle.util.encoders.Base64
import scodec.bits.ByteVector

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

object BIP322Data {
  def fromBase64String(raw64: String) = {
    val decoded = Base64.decode(raw64)
    new String(decoded)
  }

  def parse(raw: String): Try[BIP322Data] = Try(raw split "|") map {
    case Array(address, hash, sig64, "-", "-") => BIP322Data(address, ByteVector.fromValidHex(hash), sig64)
    case Array(address, hash, sig64, msg, "-") => BIP322Data(address, ByteVector.fromValidHex(hash), sig64, fromBase64String(msg).asSome)
    case Array(address, hash, sig64, msg, txid) => BIP322Data(address, ByteVector.fromValidHex(hash), sig64, fromBase64String(msg).asSome, ByteVector.fromValidHex(txid).asSome)
    case _ => throw new RuntimeException
  }
}

case class BIP322Data(address: String, messageHash: ByteVector, signature64: String, message: Option[String] = None, txid: Option[ByteVector] = None) {
  def serialize: String = s"bip322:$address|${messageHash.toHex}|$signature64|${message.map(_.getBytes).map(Base64.toBase64String) getOrElse "-"}|${txid.map(_.toHex) getOrElse "-"}"
  def hashEqualsMessage: Boolean = message.map(Bip322.getBip322MessageHash).map(ByteVector.view).forall(messageHash.==)
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
