package com.btcontract.wallet.lightning

import org.bitcoinj.crypto.{HDKeyDerivation, ChildNumber, TransactionSignature}
import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import com.btcontract.wallet.lightning.{JavaTools => jt}
import org.bitcoinj.core.{BloomFilter, Sha256Hash}
import com.btcontract.wallet.Utils.{rand, Bytes}
import org.spongycastle.jce.ECNamedCurveTable
import org.bitcoinj.wallet.DeterministicSeed
import org.bitcoinj.core.Utils.HEX
import java.math.BigInteger


object Tools { me =>
  def stringToHex(src: String) = HEX.encode(src getBytes "UTF-8")

  // Second 0 means "Bitcoin" according to BIP44
  // Deriving /M/nH/0H/<arbitrary depth> deterministic keys
  def derive(way: List[ChildNumber], n: Int, seed: DeterministicSeed) = {
    val purposeBitcoin = List(new ChildNumber(n, true), ChildNumber.ZERO_HARDENED)
    val master = HDKeyDerivation createMasterPrivateKey seed.getSeedBytes
    (master /: purposeBitcoin)(HDKeyDerivation.deriveChildKey)
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
    for (key <- identityKey +: ephemeralKeys) bloomFilter insert key
    HEX encode bloomFilter.bitcoinSerialize
  }

  // Fix signature size
  def fixSize(raw: Bytes): Bytes = raw.length match {
    case s if s < 32 => Array.fill(32 - s)(0.toByte) ++ raw
    case s if s > 32 => raw takeRight 32
    case _ => raw
  }

  // Proto signature conversion
  def bytesToSignature(bts: Bytes) = {
    val ts = TransactionSignature.decodeFromBitcoin(bts, true, true)
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
  def bytes2Sha(sha256: Bytes) = {
    val in = new ByteArrayInputStream(sha256)
    new proto.sha256_hash(jt uint64 in, jt uint64 in,
      jt uint64 in, jt uint64 in)
  }

  def sha2Bytes(ps: proto.sha256_hash) = {
    val outputStream = new ByteArrayOutputStream
    jt.writeUInt64(outputStream, ps.a, ps.b, ps.c, ps.d)
    outputStream.toByteArray
  }
}