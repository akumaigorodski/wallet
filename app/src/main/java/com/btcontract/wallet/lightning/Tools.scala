package com.btcontract.wallet.lightning

import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import com.btcontract.wallet.lightning.{JavaTools => jt}
import org.bitcoinj.crypto.TransactionSignature
import java.math.BigInteger


object Tools { me =>
  type Bytes = Array[Byte]
  val strToBytes = (_: String) getBytes "UTF-8"
  val bytesToJson = new String(_: Bytes, "UTF-8")

  // Fix signature size
  def fixSize(raw: Bytes): Bytes = raw.length match {
    case s if s < 32 => Array.fill(32 - s)(0: Byte) ++ raw
    case s if s > 32 => raw takeRight 32
    case _ => raw
  }

  // Proto signature conversion
  def bytesToSignature(bts: Bytes) = {
    val ts = TransactionSignature.decodeFromBitcoin(bts, true)
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