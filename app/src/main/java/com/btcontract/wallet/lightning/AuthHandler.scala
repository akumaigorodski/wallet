package com.btcontract.wallet.lightning

import com.softwaremill.quicklens._
import org.bitcoinj.core.{Sha256Hash, ECKey}
import com.btcontract.wallet.Utils.{Bytes, app}
import com.btcontract.wallet.lightning.{JavaTools => jt}
import com.btcontract.wallet.lightning.Tools.{ecdh, toPkt}
import com.btcontract.wallet.lightning.crypto.AeadChacha20
import com.btcontract.wallet.helper.Websocket
import org.bitcoinj.core.Utils.readUint32


object Decryptor {
  def header(buf1: Bytes, state: Decryptor) = if (buf1.length < 20) state.copy(buffer = buf1) else {
    val headLen = state.chacha.decrypt(buf1.slice(4, 20), jt writeUInt64 state.nonce, buf1 take 4, Array.empty) take 4
    add(Decryptor(state.chacha, state.nonce + 1, Array.empty, Some(readUint32(headLen, 0).toInt), state.bodies), buf1 drop 20)
  }

  def body(buf1: Bytes, state: Decryptor, headLen: Int) = if (buf1.length < headLen + 16) state.copy(buffer = buf1) else {
    val plain = state.chacha.decrypt(buf1.slice(headLen, headLen + 16), jt writeUInt64 state.nonce, buf1 take headLen, Array.empty)
    add(Decryptor(state.chacha, state.nonce + 1, Array.empty, None, state.bodies :+ plain), buf1 drop headLen + 16)
  }

  def add(state: Decryptor, data: Bytes): Decryptor = if (data.isEmpty) state else state match {
    case Decryptor(_, _, buffer, Some(headLen), bodies) => body(jt.concat(buffer, data), state, headLen)
    case Decryptor(_, _, buffer, None, _) => header(jt.concat(buffer, data), state)
  }
}

case class Encryptor(chacha: AeadChacha20, nonce: Long)
case class Decryptor(chacha: AeadChacha20, nonce: Long, buffer: Bytes = Array.empty,
                     header: Option[Int] = None, bodies: Vector[Bytes] = Vector.empty)

trait AuthState
case class NormalData(sesData: SessionData, theirNodeKey: ECKey) extends AuthState
case class SessionData(theirSesKey: Bytes, enc: Encryptor, dec: Decryptor) extends AuthState

class AuthHandler(sesKey: ECKey, socket: Websocket)
extends StateMachine[AuthState]('WaitForSesKey :: Nil, null) {
  def respond(data: Bytes, enc: Encryptor) = jt writeUInt32 data.length.toLong match { case header =>
    val (ciphertext1, mac1) = enc.chacha.encrypt(jt writeUInt64 enc.nonce, header, Array.emptyByteArray)
    val (ciphertext2, mac2) = enc.chacha.encrypt(jt writeUInt64 enc.nonce + 1, data, Array.emptyByteArray)
    socket add jt.concat(ciphertext1, mac1, ciphertext2, mac2)
    enc.modify(_.nonce).using(_ + 2)
  }

  def transfer(pack: proto.pkt) = {
    // Send packet to channel state machine
  }

  def doProcess(change: Any) = (change, data, state) match {
    // Presumably sent our handshake, waiting for their response
    case (msg: Bytes, null, 'WaitForSesKey :: rest) =>
      val theirSesPubKey = msg.slice(4, 33 + 4)

      // Generate shared secret and encryption keys
      val sharedSecret = ecdh(theirSesPubKey, sesKey.getPrivKeyBytes)
      val sendingKey = Sha256Hash hash jt.concat(sharedSecret, sesKey.getPubKey)
      val receivingKey = Sha256Hash hash jt.concat(sharedSecret, theirSesPubKey)
      val decryptor = Decryptor(new AeadChacha20(receivingKey), 0)
      val encryptor = Encryptor(new AeadChacha20(sendingKey), 0)

      // Respond with my encrypted auth info and wait for their auth
      val myPubkey = Tools bytes2BitcoinPubkey app.LNData.idKey.getPubKey
      val mySig = Tools ts2Signature app.LNData.idKey.sign(Sha256Hash twiceOf theirSesPubKey)
      val enc1 = respond(toPkt(new proto.authenticate.Builder node_id myPubkey session_sig mySig), encryptor)
      become(SessionData(theirSesPubKey, enc1, decryptor), 'waitForAuth)

    // Sent our auth data, waiting for their auth data
    case (chunk: Bytes, sd: SessionData, 'waitForAuth :: rest) =>
      val dec1 = Decryptor.add(sd.dec, chunk)

      dec1.bodies match {
        case first +: tail =>
          val protoAuth = proto.pkt.ADAPTER.decode(first).auth
          val theirSignature = Tools signature2ts protoAuth.session_sig
          val theirNodeKey = ECKey fromPublicOnly protoAuth.node_id.key.toByteArray

          // Should we do something if the tail is not empty?
          val sd1 = sd.modify(_.dec.bodies).setTo(tail).modify(_.dec.header).setTo(None)
          theirNodeKey.verifyOrThrow(Sha256Hash twiceOf sesKey.getPubKey, theirSignature)
          become(NormalData(sd1, theirNodeKey), 'normal)

        // Accumulate chunks until we get a message
        case _ => data = sd.modify(_.dec).setTo(dec1)
      }

    // Successfully authorized, now waiting for messages
    case (chunk: Bytes, nd: NormalData, 'normal :: rest) =>
      val dec1 = Decryptor.add(nd.sesData.dec, chunk)

      dec1.bodies match {
        case bodies if bodies.nonEmpty =>
          val dec2 = dec1.copy(header = None, bodies = Vector.empty)
          bodies map proto.pkt.ADAPTER.decode foreach transfer
          data = nd.modify(_.sesData.dec).setTo(dec2)

        // Again accumulate chunks until we get a message
        case _ => data = nd.modify(_.sesData.dec).setTo(dec1)
      }

    // Got a request to send a packet to counterparty
    case (message: AnyRef, nd: NormalData, 'normal :: rest) =>
      val enc1 = respond(toPkt(message), nd.sesData.enc)
      data = nd.modify(_.sesData.enc).setTo(enc1)
  }
}