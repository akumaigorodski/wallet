package com.btcontract.wallet.lightning

import java.io.{InputStream, ByteArrayInputStream, ByteArrayOutputStream, OutputStream}
import java.math.BigInteger

import org.bitcoinj.core.Sha256Hash
import org.bitcoinj.core.Utils.HEX
import org.bitcoinj.crypto.TransactionSignature


object Tools { me =>
  type Bytes = Array[Byte]

  val strToBytes = (_: String) getBytes "UTF-8"
  val bytesToJson = new String(_: Bytes, "UTF-8")

  def hash2256Hex(raw: Bytes) = HEX.encode(Sha256Hash hashTwice raw)

  // Read uint64
  def uInt64(input: InputStream): Long =
    uInt64(input.read, input.read, input.read, input.read,
      input.read, input.read, input.read, input.read)

  def uInt64(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int): Long =
    (a.&(0xffl) << 0) | (b.&(0xffl) << 8) | (c.&(0xffl) << 16) | (d.&(0xffl) << 24) |
      (e.&(0xffl) << 32) | (f.&(0xffl) << 40) | (g.&(0xffl) << 48) | (h.&(0xffl) << 56)

  // Write unit8 and unit64
  def writeUInt8(out: OutputStream, ins: Long*) =
    for (in8 <- ins) out write (in8 & 0xff).asInstanceOf[Int]

  def writeUInt64(out: OutputStream, ins: Long*) =
    for (in64 <- ins) writeUInt8(out, in64 & 0xff, (in64 >>> 8) & 0xff,
      (in64 >>> 16) & 0xff, (in64 >>> 24) & 0xff, (in64 >>> 24) & 0xff,
      (in64 >>> 32) & 0xff, (in64 >>> 40) & 0xff, (in64 >>> 48) & 0xff,
      (in64 >>> 56) & 0xff)

  // Fix signature size
  def fixSize(raw: Bytes): Bytes = raw.length match {
    case s if s < 32 => Array.fill(32 - s)(0: Byte) ++ raw
    case s if s > 32 => raw takeRight 32
    case _ => raw
  }

  // Proto signature conversion
  def bytesToSignature(bts: Bytes) = {
    val tx = TransactionSignature.decodeFromBitcoin(bts, true)
    val (arrR, arrS) = fixSize(tx.r.toByteArray).reverse -> fixSize(tx.s.toByteArray).reverse
    val (rIn, sIn) = new ByteArrayInputStream(arrR) -> new ByteArrayInputStream(arrS)
    new proto.signature(me uInt64 rIn, me uInt64 rIn, me uInt64 rIn, me uInt64 rIn,
      me uInt64 sIn, me uInt64 sIn, me uInt64 sIn, me uInt64 sIn)
  }

  def signature2Bytes(protosig: proto.signature) = {
    val (rbos, sbos) = (new ByteArrayOutputStream, new ByteArrayOutputStream)
    writeUInt64(rbos, protosig.r1, protosig.r2, protosig.r3, protosig.r4)
    writeUInt64(sbos, protosig.s1, protosig.s2, protosig.s3, protosig.s4)
    val r = new BigInteger(1, rbos.toByteArray.reverse)
    val s = new BigInteger(1, sbos.toByteArray.reverse)
    new TransactionSignature(r, s).encodeToBitcoin
  }

  // Proto sha256 conversion
  def bytes2Sha(bts: Bytes) = new ByteArrayInputStream(bts) match { case stream =>
    new proto.sha256_hash(me uInt64 stream, me uInt64 stream, me uInt64 stream, me uInt64 stream)
  }

  def sha2Bytes(sha: proto.sha256_hash) = {
    val byteOutputStream = new ByteArrayOutputStream
    writeUInt64(byteOutputStream, sha.a, sha.b, sha.c, sha.d)
    byteOutputStream.toByteArray
  }
}