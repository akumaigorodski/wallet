package fr.acinq.eclair.wire

import fr.acinq.eclair.UInt64
import fr.acinq.eclair.wire.CommonCodecs._
import fr.acinq.eclair.wire.TlvCodecs.tlvStream
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._


sealed trait OpenChannelTlv extends Tlv
sealed trait AcceptChannelTlv extends Tlv

object ChannelTlv {

  /** Commitment to where the funds will go in case of a mutual close, which remote node will enforce in case we're compromised. */
  case class UpfrontShutdownScript(script: ByteVector) extends OpenChannelTlv with AcceptChannelTlv {
    val isEmpty: Boolean = script.isEmpty
  }

}

object OpenChannelTlv {

  import ChannelTlv._

  val openTlvCodec: Codec[TlvStream[OpenChannelTlv]] = tlvStream(discriminated[OpenChannelTlv].by(varint)
    .typecase(UInt64(0), variableSizeBytesLong(varintoverflow, bytes).as[UpfrontShutdownScript])
  )

}

object AcceptChannelTlv {

  import ChannelTlv._

  val acceptTlvCodec: Codec[TlvStream[AcceptChannelTlv]] = tlvStream(discriminated[AcceptChannelTlv].by(varint)
    .typecase(UInt64(0), variableSizeBytesLong(varintoverflow, bytes).as[UpfrontShutdownScript])
  )
}