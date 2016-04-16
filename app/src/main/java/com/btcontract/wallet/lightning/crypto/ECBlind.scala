package com.btcontract.wallet.lightning.crypto

import com.btcontract.wallet.Utils.rand
import org.spongycastle.math.ec.ECPoint
import org.bitcoinj.core.Utils.HEX
import org.bitcoinj.core.ECKey
import java.math.BigInteger
import spray.json._


// As seen on http://arxiv.org/pdf/1304.2094.pdf
class ECBlind(signerQ: ECPoint, signerR: ECPoint) {
  def makeList(number: Int) = for (_ <- 1 to number) yield makeParams
  def generator = Stream continually new BigInteger(1, rand getBytes 64)

  def makeParams: BlindParams = {
    val a = new ECKey(rand).getPrivKey
    val b = new ECKey(rand).getPrivKey
    val c = new ECKey(rand).getPrivKey

    val bInv = b.modInverse(ECKey.CURVE.getN)
    val abInvQ = signerQ.multiply(a.multiply(bInv) mod ECKey.CURVE.getN)
    val blindF = signerR.multiply(bInv).add(abInvQ).add(ECKey.CURVE.getG multiply c).normalize
    val hasZeroCoords = blindF.getAffineXCoord.isZero | blindF.getAffineYCoord.isZero
    if (hasZeroCoords) makeParams else BlindParams(blindF, a, b, c, bInv)
  }
}

case class BlindParams(key: ECPoint, a: BigInteger, b: BigInteger, c: BigInteger, bInv: BigInteger) {
  def blind(msg: BigInteger) = b.multiply(keyBigInt mod ECKey.CURVE.getN).multiply(msg).add(a) mod ECKey.CURVE.getN
  def unblind(msgHat: BigInteger) = bInv.multiply(msgHat).add(c) mod ECKey.CURVE.getN
  def keyBigInt = key.getAffineXCoord.toBigInteger
}

// Turning BlindParams into JSON
object BlindParamsProtocol extends DefaultJsonProtocol { me =>
  implicit object BigIntegerFormat extends JsonFormat[BigInteger] {
    def write(bigInteger: BigInteger) = JsString apply bigInteger.toString
    def read(json: JsValue) = new BigInteger(me jsonToString json)
  }

  implicit object ECPointJson extends JsonFormat[ECPoint] {
    def write(point: ECPoint) = JsString apply HEX.encode(point getEncoded true)
    def read(json: JsValue) = ECKey.CURVE.getCurve decodePoint HEX.decode(me jsonToString json)
  }

  def jsonToString(json: JsValue) = json.convertTo[String]
  implicit val blockchainFmt = jsonFormat[ECPoint, BigInteger, BigInteger,
    BigInteger, BigInteger, BlindParams](BlindParams, "key", "a", "b", "c", "bInv")
}