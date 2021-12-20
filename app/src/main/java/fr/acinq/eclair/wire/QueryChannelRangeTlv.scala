package fr.acinq.eclair.wire

import fr.acinq.eclair.UInt64
import fr.acinq.eclair.wire.CommonCodecs.{varint, varintoverflow}
import scodec.Codec
import scodec.codecs._


sealed trait QueryChannelRangeTlv extends Tlv

object QueryChannelRangeTlv {
  case class QueryFlags(flag: Long) extends QueryChannelRangeTlv {
    val wantTimestamps = QueryFlags.wantTimestamps(flag)
    val wantChecksums = QueryFlags.wantChecksums(flag)
  }

  case object QueryFlags {
    val WANT_TIMESTAMPS: Long = 1
    val WANT_CHECKSUMS: Long = 2
    val WANT_ALL: Long = (WANT_TIMESTAMPS | WANT_CHECKSUMS)

    def wantTimestamps(flag: Long) = (flag & WANT_TIMESTAMPS) != 0
    def wantChecksums(flag: Long) = (flag & WANT_CHECKSUMS) != 0
  }

  val queryFlagsCodec: Codec[QueryFlags] = Codec(("flag" | varintoverflow)).as[QueryFlags]

  val codec: Codec[TlvStream[QueryChannelRangeTlv]] = TlvCodecs.tlvStream(discriminated.by(varint)
    .typecase(UInt64(1), variableSizeBytesLong(varintoverflow, queryFlagsCodec))
  )
}
