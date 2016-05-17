package com.btcontract.wallet.lightning

import com.btcontract.wallet.lightning.{JavaTools => jt}
import com.btcontract.wallet.lightning.crypto.AeadChacha20
import org.bitcoinj.core.Utils.readUint32
import com.btcontract.wallet.Utils.Bytes


case class Encryptor(chacha: AeadChacha20, nonce: Long)
case class Decryptor(chacha: AeadChacha20, nonce: Long, buffer: Bytes = Array.empty,
                     header: Option[Int] = None, body: Option[Bytes] = None)

object Encryptor {
  def encrypt(enc: Encryptor, data: Bytes) = jt.writeUInt32(data.length.toLong) match { case header =>
    val (ciphertext1, mac1) = enc.chacha.encrypt(jt writeUInt64 enc.nonce, header, Array.emptyByteArray)
    val (ciphertext2, mac2) = enc.chacha.encrypt(jt writeUInt64 enc.nonce + 1, data, Array.emptyByteArray)
    enc.copy(nonce = enc.nonce + 2) -> jt.concat(ciphertext1, mac1, ciphertext2, mac2)
  }
}

object Decryptor {
  def decryptHeader(buf1: Bytes, dec: Decryptor) = if (buf1.length < 20) dec.copy(buffer = buf1) else {
    val headLen = dec.chacha.decrypt(buf1.slice(4, 20), jt writeUInt64 dec.nonce, buf1 take 4, Array.empty) take 4
    val dec1 = dec.copy(nonce = dec.nonce + 1, header = Some apply readUint32(headLen, 0).toInt, buffer = Array.empty)
    add(dec1, buf1 drop 20)
  }

  def decryptBody(buf1: Bytes, dec: Decryptor, headLen: Int) = if (buf1.length < headLen) dec.copy(buffer = buf1) else {
    val plain = dec.chacha.decrypt(buf1.slice(headLen, headLen + 16), jt writeUInt64 dec.nonce, buf1 take headLen, Array.empty)
    dec.copy(nonce = dec.nonce + 1, header = dec.header, body = Some apply plain, buffer = Array.empty)
  }

  def add(dec: Decryptor, data: Bytes): Decryptor = dec match {
    case Decryptor(_, _, buffer, None, _) => decryptHeader(jt.concat(buffer, data), dec)
    case Decryptor(_, _, buffer, Some(headLen), _) => decryptBody(jt.concat(buffer, data), dec, headLen)
  }
}

class AuthHandler {

}
