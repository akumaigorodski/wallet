package com.btcontract.wallet.lightning

import com.btcontract.wallet.lightning.{JavaTools => jt}
import com.btcontract.wallet.lightning.crypto.AeadChacha20
import org.bitcoinj.core.Utils.readUint32
import com.btcontract.wallet.Utils.Bytes


case class Encryptor(chacha: AeadChacha20, nonce: Long)
case class Decryptor(chacha: AeadChacha20, nonce: Long, buffer: Bytes = Array.empty,
                     header: Option[Int] = None, bodies: Vector[Bytes] = Vector.empty)

object Encryptor {
  def encrypt(enc: Encryptor, data: Bytes) = jt.writeUInt32(data.length.toLong) match { case header =>
    val (ciphertext1, mac1) = enc.chacha.encrypt(jt writeUInt64 enc.nonce, header, Array.emptyByteArray)
    val (ciphertext2, mac2) = enc.chacha.encrypt(jt writeUInt64 enc.nonce + 1, data, Array.emptyByteArray)
    enc.copy(nonce = enc.nonce + 2) -> jt.concat(ciphertext1, mac1, ciphertext2, mac2)
  }
}

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