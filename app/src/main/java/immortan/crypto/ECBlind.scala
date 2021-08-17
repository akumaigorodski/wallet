package immortan.crypto

import fr.acinq.eclair._
import fr.acinq.bitcoin.Crypto.curve.{getCurve, getG, getN}
import fr.acinq.bitcoin.Crypto.PrivateKey
import org.bouncycastle.math.ec.ECPoint
import immortan.crypto.Tools.Bytes
import scodec.bits.ByteVector
import java.math.BigInteger


case class BlindMemo(params: List[BlindParam], clearTokens: List[BigInteger], key: String) {
  def makeBlindTokens: Seq[String] = params zip clearTokens map { case (param, token) => param.blind(token).toString }

  def makeClearSigs(blindSigs: BigInteger*): Seq[BigInteger] = params zip blindSigs map { case (param, sig) => param unblind sig }

  type ClearPointTokenSig = (String, String, String)

  def packEverything(clearSigs: BigInteger*): Seq[ClearPointTokenSig] = {
    val clearSigStrings = for (clearSig <- clearSigs) yield clearSig.toString
    val clearTokenStrings = for (clearToken <- clearTokens) yield clearToken.toString
    val blindPoints = for (param <- params) yield ByteVector.view(param.point).toHex
    (blindPoints, clearTokenStrings, clearSigStrings).zipped.toList
  }
}

// We blind a token but unblind it's signature
case class BlindParam(point: Bytes, a: BigInteger, b: BigInteger, c: BigInteger, bInv: BigInteger) {
  def blind(msg: BigInteger): BigInteger = b.multiply(keyBigInt mod getN).multiply(msg).add(a).mod(getN)

  def keyBigInt: BigInteger = getCurve.decodePoint(point).getAffineXCoord.toBigInteger

  def unblind(sigHat: BigInteger): BigInteger = bInv.multiply(sigHat).add(c).mod(getN)
}

// As seen on http://arxiv.org/pdf/1304.2094.pdf
class ECBlind(signerQ: ECPoint, signerR: ECPoint) {
  def params(number: Int): List[BlindParam] = List.fill(number)(makeParams)

  def tokens(number: Int): List[BigInteger] = List.fill(number)(oneToken)

  def oneToken = new BigInteger(1, randomBytes64.bytes.toArray)

  def makeParams: BlindParam = {
    val a: BigInteger = PrivateKey(randomBytes32).bigInt
    val b: BigInteger = PrivateKey(randomBytes32).bigInt
    val c: BigInteger = PrivateKey(randomBytes32).bigInt

    val bInv: BigInteger = b.modInverse(getN)
    val abInvQ: ECPoint = signerQ.multiply(a.multiply(bInv) mod getN)
    val blindF: ECPoint = signerR.multiply(bInv).add(abInvQ).add(getG multiply c).normalize
    if (blindF.getAffineXCoord.isZero | blindF.getAffineYCoord.isZero) makeParams
    else BlindParam(blindF.getEncoded(true), a, b, c, bInv)
  }
}

// masterPub is signerQ
class ECBlindSign(masterPriv: BigInteger) {
  val masterPrivECKey: PrivateKey = PrivateKey(masterPriv)

  def blindSign(msg: BigInteger, k: BigInteger): BigInteger = masterPriv.multiply(msg).add(k).mod(getN)

  def verifyClearSig(clearMsg: BigInteger, clearSignature: BigInteger, point: ECPoint): Boolean = {
    val rm: BigInteger = point.getAffineXCoord.toBigInteger.mod(getN).multiply(clearMsg).mod(getN)
    getG.multiply(clearSignature) == masterPrivECKey.publicKey.ecpoint.multiply(rm).add(point)
  }
}