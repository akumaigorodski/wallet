package com.btcontract.wallet.lightning

import org.bitcoinj.crypto.{HDKeyDerivation, ChildNumber, TransactionSignature}
import com.btcontract.wallet.Utils.{rand, none, runAnd, Bytes}
import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import javax.crypto.spec.{SecretKeySpec, IvParameterSpec}
import org.bitcoinj.core.{ECKey, BloomFilter, Sha256Hash}
import com.btcontract.wallet.lightning.{JavaTools => jt}

import org.spongycastle.jce.ECNamedCurveTable
import org.bitcoinj.core.ECKey.ECDSASignature
import org.bitcoinj.wallet.DeterministicSeed
import org.bitcoinj.core.Utils.HEX
import java.math.BigInteger
import javax.crypto.Cipher
import okio.ByteString


object Tools { me =>
  type PktVec = Vector[proto.pkt]
  def has(value: Any) = value != null
  def humanIdentity(key: ECKey) = key.getPublicKeyAsHex grouped 5 mkString "\u0020"
  def decodeSignature(bts: Bytes) = TransactionSignature.decodeFromBitcoin(bts, true, true)
  val pre2HashProto = sha2Bytes _ andThen Sha256Hash.hash andThen bytes2Sha
  val r2HashProto = rval2Bytes _ andThen Sha256Hash.hash andThen bytes2Sha
  val bytes2bs = (bytes: Bytes) => ByteString.of(bytes, 0, bytes.length)

  // Bloom filter for incoming Requests and Responses
  def mkBloom(ephemeralKeys: Seq[Bytes], identityKey: Bytes) = {
    val bloomFilter = new BloomFilter(ephemeralKeys.size + 1, 0.00001, rand.nextInt)
    for (nextKey <- identityKey +: ephemeralKeys) bloomFilter insert nextKey
    HEX encode bloomFilter.bitcoinSerialize
  }

  // Wrap inner protobuf messages into a pkt
  def toPkt(some: AnyRef) = (new proto.pkt.Builder, some) match {
    case (bld, con: proto.update_fulfill_htlc) => bld.update_fulfill_htlc(con).build
    case (bld, con: proto.update_revocation) => bld.update_revocation(con).build
    case (bld, con: proto.update_fail_htlc) => bld.update_fail_htlc(con).build
    case (bld, con: proto.update_add_htlc) => bld.update_add_htlc(con).build
    case (bld, con: proto.update_commit) => bld.update_commit(con).build
    case (bld, con: proto.update_fee) => bld.update_fee(con).build

    case (bld, con: proto.close_signature) => bld.close_signature(con).build
    case (bld, con: proto.close_shutdown) => bld.close_shutdown(con).build

    case (bld, con: proto.open_commit_sig) => bld.open_commit_sig(con).build
    case (bld, con: proto.open_complete) => bld.open_complete(con).build
    case (bld, con: proto.open_anchor) => bld.open_anchor(con).build
    case (bld, con: proto.open_channel) => bld.open(con).build

    case (bld, con: proto.reconnect) => bld.reconnect(con).build
    case (bld, con: proto.authenticate) => bld.auth(con).build
    case (bld, con: proto.error) => bld.error(con).build
    case _ => throw new Exception(s"$some unknown")
  }

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
    val x = mult.getXCoord.toBigInteger.toByteArray.takeRight(32)
    Sha256Hash.hash(prefix.toByte +: x)
  }

  def proto2ECKey(message: proto.bitcoin_pubkey) = ECKey fromPublicOnly message.key.toByteArray
  def bytes2ProtoPubkey(bytes: Bytes) = new proto.bitcoin_pubkey(me bytes2bs bytes)
  def locktime2Blocks(locktime: proto.locktime) = locktime.blocks.toInt
  def blocks2Locktime(num: Int) = new proto.locktime(null, num)

  // Fix signature size
  def fixSize(raw: Bytes): Bytes = raw.length match {
    case s if s < 32 => jt.concat(Array.fill(32 - s)(0.toByte), raw)
    case s if s > 32 => raw takeRight 32
    case _ => raw
  }

  // Proto signature conversion
  def ts2Signature(ts: ECDSASignature) = {
    val inR = new ByteArrayInputStream(me fixSize ts.r.toByteArray)
    val inS = new ByteArrayInputStream(me fixSize ts.s.toByteArray)
    new proto.signature(jt uint64 inR, jt uint64 inR, jt uint64 inR,
      jt uint64 inR, jt uint64 inS, jt uint64 inS,
      jt uint64 inS, jt uint64 inS)
  }

  def signature2Ts(protosig: proto.signature) = {
    val (rbos, sbos) = (new ByteArrayOutputStream, new ByteArrayOutputStream)
    jt.writeUInt64(rbos, protosig.r1, protosig.r2, protosig.r3, protosig.r4)
    jt.writeUInt64(sbos, protosig.s1, protosig.s2, protosig.s3, protosig.s4)
    val r = new BigInteger(1, rbos.toByteArray)
    val s = new BigInteger(1, sbos.toByteArray)
    new TransactionSignature(r, s)
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

  // Proto rval conversion
  def bytes2Rval(rval: Bytes) = new ByteArrayInputStream(rval) match { case in =>
    new proto.rval(jt uint64 in, jt uint64 in, jt uint64 in, jt uint64 in)
  }

  def rval2Bytes(rv: proto.rval) = {
    val outputStream = new ByteArrayOutputStream
    jt.writeUInt64(outputStream, rv.a, rv.b, rv.c, rv.d)
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

// A general purpose State Machine
abstract class StateMachine[T](var state: List[Symbol], var data: T) { me =>
  def process(change: Any) = try me synchronized doProcess(change) catch error
  def become(fresh: T, ns: Symbol) = runAnd(state = ns :: state take 2)(data = fresh)
  def stayWith(dataOnly: T) = become(dataOnly, state.head)
  def error: PartialFunction[Throwable, Unit] = none
  def doProcess(change: Any)
}