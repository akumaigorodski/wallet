package com.btcontract.wallet.lightning

import org.bitcoinj.crypto.{HDKeyDerivation, ChildNumber, TransactionSignature}
import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import com.btcontract.wallet.Utils.{rand, wrap, none, Bytes}
import javax.crypto.spec.{SecretKeySpec, IvParameterSpec}
import com.btcontract.wallet.lightning.{JavaTools => jt}
import org.bitcoinj.core.{BloomFilter, Sha256Hash}

import org.spongycastle.jce.ECNamedCurveTable
import org.bitcoinj.core.ECKey.ECDSASignature
import com.btcontract.wallet.lightning.proto
import org.bitcoinj.wallet.DeterministicSeed
import org.bitcoinj.core.Utils.HEX
import java.math.BigInteger
import javax.crypto.Cipher
import okio.ByteString


object Tools { me =>
  def uuid = HEX.encode(rand getBytes 64)
  def stringToHex(src: String) = HEX.encode(src getBytes "UTF-8")
  def decodeSignature(bts: Bytes) = TransactionSignature.decodeFromBitcoin(bts, true, true)

  // Second 0 means "Bitcoin" according to BIP44
  // Deriving /M/nH/0H/<arbitrary depth> deterministic keys
  def derive(way: List[ChildNumber], n: Int)(seed: DeterministicSeed) = {
    val masterKey = HDKeyDerivation createMasterPrivateKey seed.getSeedBytes
    val purposeBitcoin = List(new ChildNumber(n, true), ChildNumber.ZERO_HARDENED)
    (purposeBitcoin ::: way).foldLeft(masterKey)(HDKeyDerivation.deriveChildKey)
  }

  // Shared secret for secp256k1
  def ecdh(pub: Bytes, priv: Bytes) = {
    val ecSpec = ECNamedCurveTable getParameterSpec "secp256k1"
    val pubPoint = ecSpec.getCurve decodePoint pub
    val bigPriv = new BigInteger(1, priv)

    val mult = pubPoint.multiply(bigPriv).normalize
    val prefix = if (mult.getYCoord.toBigInteger testBit 0) 0x03 else 0x02
    Sha256Hash hash prefix.toByte +: mult.getXCoord.toBigInteger.toByteArray.takeRight(32)
  }

  // Bloom filter for incoming Requests and Responses
  def mkBloom(ephemeralKeys: Seq[Bytes], identityKey: Bytes) = {
    val bloomFilter = new BloomFilter(ephemeralKeys.size + 1, 0.00001, rand.nextInt)
    for (nextKey <- identityKey +: ephemeralKeys) bloomFilter insert nextKey
    HEX encode bloomFilter.bitcoinSerialize
  }

  // Fix signature size
  def fixSize(raw: Bytes): Bytes = raw.length match {
    case s if s < 32 => jt.concat(Array.fill(32 - s)(0.toByte), raw)
    case s if s > 32 => raw takeRight 32
    case _ => raw
  }

  // PubKey bytes to proto
  def bytes2BitcoinPubkey(bytes: Bytes) = {
    val bitcoinPubkey = new proto.bitcoin_pubkey.Builder
    val bs = ByteString.of(LNSeed.idKey.getPubKey:_*)
    bitcoinPubkey.key(bs).build
  }

  // Proto signature conversion
  def tsToSignature(ts: ECDSASignature) = {
    val inR = new ByteArrayInputStream(fixSize(ts.r.toByteArray).reverse)
    val inS = new ByteArrayInputStream(fixSize(ts.s.toByteArray).reverse)
    new proto.signature(jt uint64 inR, jt uint64 inR, jt uint64 inR,
      jt uint64 inR, jt uint64 inS, jt uint64 inS,
      jt uint64 inS, jt uint64 inS)
  }

  def signature2Bytes(protosig: proto.signature) = {
    val (rbos, sbos) = (new ByteArrayOutputStream, new ByteArrayOutputStream)
    jt.writeUInt64(rbos, protosig.r1, protosig.r2, protosig.r3, protosig.r4)
    jt.writeUInt64(sbos, protosig.s1, protosig.s2, protosig.s3, protosig.s4)
    val r = new BigInteger(1, rbos.toByteArray.reverse)
    val s = new BigInteger(1, sbos.toByteArray.reverse)
    new TransactionSignature(r, s).encodeToBitcoin
  }

  // Proto sha256 conversion
  def bytes2Sha(sha256: Bytes) = new ByteArrayInputStream(sha256) match { case in =>
    new proto.sha256_hash(jt uint64 in, jt uint64 in, jt uint64 in, jt uint64 in)
  }

  def sha2Bytes(ps: proto.sha256_hash) = {
    val outputStream = new ByteArrayOutputStream
    jt.writeUInt64(outputStream, ps.a, ps.b, ps.c, ps.d)
    outputStream.toByteArray
  }
}

object AES {
  def cipher(key: Bytes, initVector: Bytes, mode: Int) =
    Cipher getInstance "AES/CTR/NoPadding" match { case aesCipher =>
      val ivParameterSpec: IvParameterSpec = new IvParameterSpec(initVector)
      aesCipher.init(mode, new SecretKeySpec(key, "AES"), ivParameterSpec)
      aesCipher
    }

  def encCypher(key: Bytes, initVector: Bytes) = cipher(key, initVector, Cipher.ENCRYPT_MODE)
  def decCypher(key: Bytes, initVector: Bytes) = cipher(key, initVector, Cipher.DECRYPT_MODE)
  def enc(data: Bytes, key: Bytes, initVector: Bytes) = encCypher(key, initVector) doFinal data
  def dec(data: Bytes, key: Bytes, initVector: Bytes) = decCypher(key, initVector) doFinal data
}

object LNSeed {
  private var seed: DeterministicSeed = null
  // Commit tx keys are located at /M/100H/0H/x, id will be at /M/101H/0H/0
  def commitKey(x: Int) = Tools.derive(new ChildNumber(x) :: Nil, 100)(seed)
  lazy val idKey = Tools.derive(new ChildNumber(0) :: Nil, 101)(seed)
  def setSeed(newSeed: DeterministicSeed) = seed = newSeed
  def seedAbsent = seed == null
}

// A general purpose State Machine
abstract class StateMachine[T](var state: List[Symbol], var data: T) { me =>
  def become(nData: T, ns: Symbol) = wrap { data = nData } { state = ns :: state take 3 }
  def process(change: Any) = try synchronized(me doProcess change) catch error
  def error: PartialFunction[Throwable, Unit] = none
  def doProcess(change: Any)
}