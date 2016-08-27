package com.btcontract.wallet.lightning.lncloud

import spray.json._
import org.bitcoinj.core.{Sha256Hash, ECKey}

import org.bitcoinj.core.ECKey.ECDSASignature
import com.btcontract.wallet.Utils.Bytes
import org.spongycastle.math.ec.ECPoint
import org.bitcoinj.core.Utils.HEX
import java.math.BigInteger


object ThundercloudProtocol extends DefaultJsonProtocol { me =>
  implicit object BigIntegerFormat extends JsonFormat[BigInteger] {
    def write(bigInteger: BigInteger) = JsString apply bigInteger.toString
    def read(json: JsValue) = new BigInteger(me jsonToString json)
  }

  implicit object ECPointJson extends JsonFormat[ECPoint] {
    def write(point: ECPoint) = JsString apply HEX.encode(point getEncoded true)
    def read(json: JsValue) = ECKey.CURVE.getCurve decodePoint HEX.decode(me jsonToString json)
  }

  def jsonToString(json: JsValue) = json.convertTo[String]
  implicit val blindParamFmt = jsonFormat[ECPoint, BigInteger, BigInteger,
    BigInteger, BigInteger, BlindParam](BlindParam, "key", "a", "b", "c", "bInv")

  // Remember blind tokens settings in memory
  implicit val blindMemoFmt = jsonFormat[List[BlindParam], List[BigInteger], String,
    String, BlindMemo](BlindMemo, "params", "clears", "sesKeyHex", "rHash")

  // Request and Charge which can be remote or NFC-based
  implicit val requestFmt = jsonFormat[Option[Bytes], Long, String, String,
    Request](Request, "ephemeral", "mSatAmount", "message", "id")

  implicit val chargeFmt = jsonFormat[Request, Bytes,
    Charge](Charge, "request", "lnPaymentData")

  // Message and Wrap
  implicit val messageFmt = jsonFormat[Bytes, Bytes, Message](Message, "pubKey", "content")
  implicit val wrapFmt = jsonFormat[Message, Long, Wrap](Wrap, "data", "stamp")

  // WatchdogTx for temote observing of channel breaches
  implicit val watchdogTxFmt = jsonFormat[String, String, String,
    WatchdogTx](WatchdogTx, "prefix", "txEnc", "iv")

  // User signed email to key mapping
  implicit val smFmt = jsonFormat[String, String, String,
    SignedMail](SignedMail, "email", "pubKey", "signature")
}

// A "response-to" ephemeral key, it's private part should be stored in a database
// because my bloom filter has it, it's optional because Charge may come locally via NFC
case class Request(ephemeral: Option[Bytes], mSatAmount: Long, message: String, id: String)
case class Charge(request: Request, lnPaymentData: Bytes)

// Request/Response container and Wrap
case class Message(pubKey: Bytes, content: Bytes)
case class Wrap(data: Message, stamp: Long)

// Prefix is first 16 bytes of txId, key is last 16 bytes
case class WatchdogTx(prefix: String, txEnc: String, iv: String)

// Client signed email
case class SignedMail(email: String, pubKey: String, signature: String) {
  def totalHash = Sha256Hash.of(email + pubKey + signature getBytes "UTF-8")
  def identityPubECKey = ECKey.fromPublicOnly(HEX decode pubKey)
  def emailHash = Sha256Hash.of(email getBytes "UTF-8")

  def checkSig = HEX decode signature match { case sig =>
    identityPubECKey.verify(emailHash, ECDSASignature decodeFromDER sig)
  }
}