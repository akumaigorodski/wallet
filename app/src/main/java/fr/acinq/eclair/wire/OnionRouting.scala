package fr.acinq.eclair.wire

import fr.acinq.bitcoin.ByteVector32
import fr.acinq.eclair.UInt64
import fr.acinq.eclair.wire.CommonCodecs.bytes32
import scodec.bits.ByteVector
import scodec.codecs._
import scodec.{Codec, Err}


case class OnionRoutingPacket(version: Int, publicKey: ByteVector, payload: ByteVector, hmac: ByteVector32)

object OnionRoutingCodecs {

  case class MissingRequiredTlv(tag: UInt64) extends Err {
    // @formatter:off
    val failureMessage: FailureMessage = InvalidOnionPayload(tag, 0)
    override def message = failureMessage.message
    override def context: List[String] = Nil
    override def pushContext(ctx: String): Err = this
    // @formatter:on
  }

  def onionRoutingPacketCodec(payloadLength: Int): Codec[OnionRoutingPacket] = (
    ("version" | uint8) ::
      ("publicKey" | bytes(33)) ::
      ("onionPayload" | bytes(payloadLength)) ::
      ("hmac" | bytes32)).as[OnionRoutingPacket]

}