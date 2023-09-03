package fr.acinq.eclair.wire

import java.net.{Inet4Address, Inet6Address, InetAddress}

import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.DeterministicWallet.{ExtendedPrivateKey, ExtendedPublicKey, KeyPath}
import fr.acinq.bitcoin.{ByteVector32, ByteVector64, OutPoint, Transaction}
import fr.acinq.eclair.UInt64
import org.apache.commons.codec.binary.Base32
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

import scala.Ordering.Implicits._
import scala.util.Try


object CommonCodecs {
  def discriminatorWithDefault[A](discriminator: Codec[A], fallback: Codec[A]): Codec[A] = new Codec[A] {
    def sizeBound: SizeBound = discriminator.sizeBound | fallback.sizeBound

    def encode(e: A): Attempt[BitVector] = discriminator.encode(e).recoverWith {
      case _ => fallback.encode(e)
    }

    def decode(b: BitVector): Attempt[DecodeResult[A]] = discriminator.decode(b).recoverWith {
      case _: KnownDiscriminatorType[_]#UnknownDiscriminator => fallback.decode(b)
    }
  }

  def minimalvalue[A: Ordering](codec: Codec[A], min: A): Codec[A] = codec.exmap(f = {
    case i if i < min => Attempt failure Err("value was not minimally encoded")
    case i => Attempt.successful(i)
  }, Attempt.successful)

  val uint64overflow: Codec[Long] = int64.narrow(l => if (l >= 0) Attempt.Successful(l) else Attempt failure Err(s"overflow for value $l"), identity)

  val uint64: Codec[UInt64] = bytes(8).xmap(UInt64.apply, _.toByteVector padLeft 8)

  val text: Codec[String] = variableSizeBytes(uint16, utf8)

  val varint: Codec[UInt64] = discriminatorWithDefault(
    discriminated[UInt64].by(uint8L)
      .\(0xff) { case i if i >= UInt64(0x100000000L) => i }(minimalvalue(uint64, UInt64(0x100000000L)))
      .\(0xfe) { case i if i >= UInt64(0x10000) => i }(minimalvalue(uint32.xmap(UInt64(_), _.toBigInt.toLong), UInt64(0x10000)))
      .\(0xfd) { case i if i >= UInt64(0xfd) => i }(minimalvalue(uint16.xmap(UInt64(_), _.toBigInt.toInt), UInt64(0xfd))),
    uint8L.xmap(UInt64(_), _.toBigInt.toInt)
  )

  val varintoverflow: Codec[Long] = varint.narrow(l => if (UInt64(Long.MaxValue) > l) Attempt failure Err(s"overflow for value $l") else Attempt.successful(l.toBigInt.toLong), UInt64.apply)

  def lengthDelimited[T](codec: Codec[T]): Codec[T] = variableSizeBytesLong(varintoverflow, codec)

  val outPointCodec: Codec[OutPoint] = lengthDelimited {
    bytes.xmap(d => OutPoint.read(d.toArray), OutPoint.write)
  }

  val txCodec: Codec[Transaction] = lengthDelimited {
    bytes.xmap(d => Transaction.read(d.toArray), Transaction.write)
  }

  val bytes32: Codec[ByteVector32] = limitedSizeBytes(codec = bytesStrict(32).xmap(ByteVector32.apply, _.bytes), limit = 32)

  val bytes64: Codec[ByteVector64] = limitedSizeBytes(codec = bytesStrict(64).xmap(ByteVector64.apply, _.bytes), limit = 64)

  val varsizebinarydata: Codec[ByteVector] = variableSizeBytes(uint16, bytes)

  val ipv4address: Codec[Inet4Address] = bytes(4).xmap(b => InetAddress.getByAddress(b.toArray).asInstanceOf[Inet4Address], a => ByteVector(a.getAddress))

  val ipv6address: Codec[Inet6Address] = bytes(16).exmap(b => Attempt.fromTry(Try(Inet6Address.getByAddress(null, b.toArray, null))), a => Attempt.fromTry(Try(ByteVector(a.getAddress))))

  def base32(size: Int): Codec[String] = bytes(size).xmap(b => new Base32().encodeAsString(b.toArray).toLowerCase, a => ByteVector(new Base32().decode(a.toUpperCase)))

  val nodeaddress: Codec[NodeAddress] =
    discriminated[NodeAddress].by(uint8)
      .typecase(1, (ipv4address :: uint16).as[IPv4])
      .typecase(2, (ipv6address :: uint16).as[IPv6])
      .typecase(3, (base32(10) :: uint16).as[Tor2])
      .typecase(4, (base32(35) :: uint16).as[Tor3])
      .typecase(5, (zeropaddedstring(64) :: uint16).as[Domain])

  val listofnodeaddresses: Codec[List[NodeAddress]] = variableSizeBytes(uint16, list(nodeaddress))

  val privateKey: Codec[PrivateKey] = Codec[PrivateKey](
    (priv: PrivateKey) => bytes(32).encode(priv.value),
    (wire: BitVector) => bytes(32).decode(wire).map(_.map(PrivateKey.apply))
  )

  val publicKey: Codec[PublicKey] = Codec[PublicKey](
    (pub: PublicKey) => bytes(33).encode(pub.value),
    (wire: BitVector) => bytes(33).decode(wire).map(_.map(PublicKey.apply))
  )

  def zeropaddedstring(size: Int): Codec[String] = fixedSizeBytes(size, utf8).xmap(_.takeWhile(_ != '\u0000'), identity)

  val keyPathCodec: Codec[KeyPath] = (listOfN(uint16, uint32) withContext "path").xmap[KeyPath](KeyPath.apply, _.path.toList).as[KeyPath]

  val extendedPrivateKeyCodec: Codec[ExtendedPrivateKey] = {
    ("secretkeybytes" | bytes32) ::
      ("chaincode" | bytes32) ::
      ("depth" | uint16) ::
      ("path" | keyPathCodec) ::
      ("parent" | int64)
  }.as[ExtendedPrivateKey]

  val extendedPublicKeyCodec: Codec[ExtendedPublicKey] = {
    ("publickeybytes" | varsizebinarydata) ::
      ("chaincode" | bytes32) ::
      ("depth" | uint16) ::
      ("path" | keyPathCodec) ::
      ("parent" | int64)
  }.as[ExtendedPublicKey]
}
