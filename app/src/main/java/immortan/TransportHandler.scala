package immortan

import java.nio.ByteOrder
import java.util.concurrent.Executors

import fr.acinq.bitcoin.Protocol
import fr.acinq.eclair.wire.{LightningMessage, LightningMessageCodecs}
import immortan.TransportHandler._
import immortan.crypto.Noise._
import immortan.crypto.StateMachine
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}


// Used to decrypt remote messages -> send to channel as well as encrypt outgoing messages -> send to socket
abstract class TransportHandler(keyPair: KeyPair, remotePubKey: ByteVector) extends StateMachine[Data] { me =>
  implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  def process(change: Any): Unit = Future(me doProcess change)

  def handleDecryptedIncomingData(data: ByteVector): Unit
  def handleEncryptedOutgoingData(data: ByteVector): Unit
  def handleEnterOperationalState: Unit

  def init: Unit = {
    val writer = makeWriter(keyPair, remotePubKey)
    val (reader, _, msg) = writer.write(ByteVector.empty)
    become(HandshakeData(reader, ByteVector.empty), HANDSHAKE)
    handleEncryptedOutgoingData(prefix +: msg)
  }

  def UPDATE(d1: Data): Unit = become(d1, state)

  def doProcess(change: Any): Unit = (data, change, state) match {
    case (HandshakeData(reader1, buffer), bv: ByteVector, HANDSHAKE) =>
      me UPDATE HandshakeData(reader1, buffer ++ bv)
      doProcess(Ping)

    case (HandshakeData(reader1, buffer), Ping, HANDSHAKE) if buffer.length >= expectedLength(reader1) =>
      require(buffer.head == prefix, s"Invalid prefix=${buffer.head} in handshake buffer")
      val (payload, remainder) = buffer.tail.splitAt(expectedLength(reader1) - 1)

      reader1 read payload match {
        case (_, (decoder, encoder, ck), _) =>
          val encoder1 = ExtendedCipherState(encoder, ck)
          val decoder1 = ExtendedCipherState(decoder, ck)
          val d1 = CyphertextData(encoder1, decoder1, None, remainder)
          become(d1, WAITING_CYPHERTEXT)
          handleEnterOperationalState
          doProcess(Ping)

        case (writer, _, _) =>
          writer write ByteVector.empty match {
            case (_, (encoder, decoder, ck), message) =>
              val encoder1 = ExtendedCipherState(encoder, ck)
              val decoder1 = ExtendedCipherState(decoder, ck)
              val d1 = CyphertextData(encoder1, decoder1, None, remainder)
              handleEncryptedOutgoingData(prefix +: message)
              become(d1, WAITING_CYPHERTEXT)
              handleEnterOperationalState
              doProcess(Ping)

            case (reader2, _, message) =>
              handleEncryptedOutgoingData(prefix +: message)
              become(HandshakeData(reader2, remainder), HANDSHAKE)
              doProcess(Ping)
          }
      }

    // Normal operation phase: messages can be sent and received here
    case (cd: CyphertextData, msg: LightningMessage, WAITING_CYPHERTEXT) =>
      val encoded = LightningMessageCodecs.lightningMessageCodecWithFallback.encode(LightningMessageCodecs prepare msg)
      val (encoder1, ciphertext) = encryptMsg(cd.enc, encoded.require.toByteVector)
      handleEncryptedOutgoingData(ciphertext)
      me UPDATE cd.copy(enc = encoder1)

    case (cd: CyphertextData, bv: ByteVector, WAITING_CYPHERTEXT) =>
      me UPDATE cd.copy(buffer = cd.buffer ++ bv)
      doProcess(Ping)

    case (CyphertextData(encoder, decoder, None, buffer), Ping, WAITING_CYPHERTEXT) if buffer.length >= 18 =>
      val (ciphertext, remainder) = buffer.splitAt(18)
      val (decoder1, plaintext) = decoder.decryptWithAd(ByteVector.empty, ciphertext)
      val length = Some apply Protocol.uint16(plaintext.toArray, ByteOrder.BIG_ENDIAN)
      me UPDATE CyphertextData(encoder, decoder1, length, remainder)
      doProcess(Ping)

    case (CyphertextData(encoder, decoder, Some(length), buffer), Ping, WAITING_CYPHERTEXT) if buffer.length >= length + 16 =>
      val (ciphertext, remainder) = buffer.splitAt(length + 16)
      val (decoder1, plaintext) = decoder.decryptWithAd(ByteVector.empty, ciphertext)
      me UPDATE CyphertextData(encoder, decoder1, length = None, remainder)
      handleDecryptedIncomingData(plaintext)
      doProcess(Ping)

    case _ =>
  }
}

object TransportHandler {
  val prologue: ByteVector = ByteVector("lightning" getBytes "UTF-8")
  val prefix: Byte = 0.toByte
  val Ping = "Ping"

  val HANDSHAKE = 0
  val WAITING_CYPHERTEXT = 1

  def expectedLength(reader: HandshakeStateReader): Int = reader.messages.length match { case 3 | 2 => 50 case _ => 66 }

  def encryptMsg(enc: CipherState, plaintext: ByteVector): (CipherState, ByteVector) = {
    val plaintextAsUinteger16 = Protocol.writeUInt16(plaintext.length.toInt, ByteOrder.BIG_ENDIAN)
    val (enc1, ciphertext1) = enc.encryptWithAd(ByteVector.empty, plaintextAsUinteger16)
    val (enc2, ciphertext2) = enc1.encryptWithAd(ByteVector.empty, plaintext)
    (enc2, ciphertext1 ++ ciphertext2)
  }

  private def makeWriter(localStatic: KeyPair, remoteStatic: ByteVector) =
    HandshakeState.initializeWriter(handshakePatternXK, prologue, localStatic,
      KeyPair(ByteVector.empty, ByteVector.empty), remoteStatic, ByteVector.empty,
      Secp256k1DHFunctions, Chacha20Poly1305CipherFunctions, SHA256HashFunctions)

  sealed trait Data
  case class HandshakeData(reader: HandshakeStateReader, buffer: ByteVector) extends Data
  case class CyphertextData(enc: CipherState, dec: CipherState, length: Option[Int], buffer: ByteVector) extends Data
}

// A key is to be rotated after a party sends of decrypts 1000 messages with it
case class ExtendedCipherState(cs: CipherState, ck: ByteVector) extends CipherState { me =>
  def encryptWithAd(ad: ByteVector, plaintext: ByteVector): (ExtendedCipherState, ByteVector) =
    cs match {
      case InitializedCipherState(k, 999, _) =>
        val (_, ciphertext) = cs.encryptWithAd(ad, plaintext)
        val (chainKey1, material1) = SHA256HashFunctions.hkdf(ck, k)
        copy(cs = cs initializeKey material1, ck = chainKey1) -> ciphertext

      case _: InitializedCipherState =>
        val (cs1, ciphertext) = cs.encryptWithAd(ad, plaintext)
        copy(cs = cs1) -> ciphertext

      case _: UnitializedCipherState =>
        me -> plaintext
    }

  def decryptWithAd(ad: ByteVector, ciphertext: ByteVector): (ExtendedCipherState, ByteVector) =
    cs match {
      case InitializedCipherState(k, 999, _) =>
        val (_, plaintext) = cs.decryptWithAd(ad, ciphertext)
        val (chainKey1, material1) = SHA256HashFunctions.hkdf(ck, k)
        copy(cs = cs initializeKey material1, ck = chainKey1) -> plaintext

      case _: InitializedCipherState =>
        val (cs1, plaintext) = cs.decryptWithAd(ad, ciphertext)
        copy(cs = cs1) -> plaintext

      case _: UnitializedCipherState =>
        me -> ciphertext
    }

  def cipher: CipherFunctions = cs.cipher
  val hasKey: Boolean = cs.hasKey
}