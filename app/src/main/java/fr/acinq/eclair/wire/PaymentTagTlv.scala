package fr.acinq.eclair.wire

import fr.acinq.bitcoin.ByteVector32
import fr.acinq.eclair.wire.CommonCodecs._
import scodec._
import scodec.bits.ByteVector
import scodec.codecs._


case class FullPaymentTag(paymentHash: ByteVector32, paymentSecret: ByteVector32, tag: Int)

case class ShortPaymentTag(paymentSecret: ByteVector32, tag: Int)

case class EncryptedPaymentSecret(data: ByteVector) extends Tlv

object PaymentTagTlv {
  final val LOCALLY_SENT = 0
  final val TRAMPLOINE_ROUTED = 1
  final val FINAL_INCOMING = 2

  type EncryptedSecretStream = TlvStream[EncryptedPaymentSecret]

  val shortPaymentTagCodec = {
    (bytes32 withContext "paymentSecret") ::
      (int32 withContext "tag")
  }.as[ShortPaymentTag]

  private val encryptedPaymentSecretCodec = variableSizeBytesLong(varintoverflow, bytes).as[EncryptedPaymentSecret]

  private val discriminator = discriminated[EncryptedPaymentSecret].by(varint).typecase(TlvStream.paymentTag, encryptedPaymentSecretCodec)

  val codec: Codec[EncryptedSecretStream] = TlvCodecs.tlvStream(discriminator)
}