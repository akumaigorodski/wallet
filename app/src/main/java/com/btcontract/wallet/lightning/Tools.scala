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

  // Read uint64
  def uint64(input: InputStream): Long =
    uint64(input.read, input.read, input.read, input.read,
      input.read, input.read, input.read, input.read)

  def uint64(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int) =
    a.&(0xffl).<<(0) | b.&(0xffl).<<(8) | c.&(0xffl).<<(16) | d.&(0xffl).<<(24) |
      e.&(0xffl).<<(32) | f.&(0xffl).<<(40) | g.&(0xffl).<<(48) | h.&(0xffl).<<(56)

  // Write unit8 and unit64
  def writeUInt8(out: OutputStream, inputs: Long*) =
    for (input <- inputs) out write (input & 0xff).asInstanceOf[Int]

  def writeUInt64(out: OutputStream, inputs: Long*) =
    for (input <- inputs) writeUInt8(out, input & 0xff, (input >>> 8) & 0xff,
      (input >>> 16) & 0xff, (input >>> 24) & 0xff, (input >>> 32) & 0xff,
      (input >>> 40) & 0xff, (input >>> 48) & 0xff, (input >>> 56) & 0xff)

  def writeUInt64(input: Long): Bytes = {
    val output = new ByteArrayOutputStream(8)
    writeUInt64(output, input)
    output.toByteArray
  }

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
    new proto.signature(me uint64 inR, me uint64 inR, me uint64 inR,
      me uint64 inR, me uint64 inS, me uint64 inS,
      me uint64 inS, me uint64 inS)
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
  def bytes2Sha(sha256hashByteArray: Bytes) = {
    val in = new ByteArrayInputStream(sha256hashByteArray)
    new proto.sha256_hash(me uint64 in, me uint64 in, me uint64 in, me uint64 in)
  }

  def sha2Bytes(ps: proto.sha256_hash) = {
    val outputStream = new ByteArrayOutputStream
    writeUInt64(outputStream, ps.a, ps.b, ps.c, ps.d)
    outputStream.toByteArray
  }
}