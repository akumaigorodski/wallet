package fr.acinq.eclair.wire

import fr.acinq.eclair.UInt64
import scodec.bits.ByteVector

import scala.reflect.ClassTag


trait Tlv

/**
 * Generic tlv type we fallback to if we don't understand the incoming tlv.
 *
 * @param tag   tlv tag.
 * @param value tlv value (length is implicit, and encoded as a varint).
 */
case class GenericTlv(tag: UInt64, value: ByteVector) extends Tlv

/**
 * A tlv stream is a collection of tlv records.
 * A tlv stream is constrained to a specific tlv namespace that dictates how to parse the tlv records.
 * That namespace is provided by a trait extending the top-level tlv trait.
 *
 * @param records known tlv records.
 * @param unknown unknown tlv records.
 * @tparam T the stream namespace is a trait extending the top-level tlv trait.
 */
case class TlvStream[T <: Tlv](records: Traversable[T], unknown: Traversable[GenericTlv] = Nil) {
  /**
   *
   * @tparam R input type parameter, must be a subtype of the main TLV type
   * @return the TLV record of type that matches the input type parameter if any (there can be at most one, since BOLTs specify
   *         that TLV records are supposed to be unique)
   */
  def get[R <: T : ClassTag]: Option[R] = records.collectFirst { case r: R => r }
}

object TlvStream {
  type GenericTlvStream = TlvStream[Tlv]

  def empty[T <: Tlv]: TlvStream[T] = TlvStream[T](Nil, Nil)

  def apply[T <: Tlv](records: T*): TlvStream[T] = TlvStream(records, Nil)

  // Payment type

  final val paymentTag = UInt64(4127926135L)
}