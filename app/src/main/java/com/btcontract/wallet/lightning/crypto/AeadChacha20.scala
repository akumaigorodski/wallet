package com.btcontract.wallet.lightning.crypto

import com.btcontract.wallet.lightning.{JavaTools => jt}
import org.spongycastle.crypto.params.ParametersWithIV
import org.spongycastle.crypto.engines.ChaChaEngine
import org.spongycastle.crypto.params.KeyParameter
import com.btcontract.wallet.Utils.Bytes


class AeadChacha20(asymmetricKey: Bytes) {
  def chacha20Run(content: Bytes, nonce: Bytes, encrypt: Boolean, skip: Boolean) = {
    val cipherParameters = new ParametersWithIV(new KeyParameter(asymmetricKey), nonce)
    val finalResult = new Bytes(content.length)
    val engine = new ChaChaEngine(20)

    engine.init(encrypt, cipherParameters)
    // skip 1 block == set counter to 1 instead of 0
    if (skip) engine.processBytes(new Bytes(64), 0, 64, new Bytes(64), 0)
    engine.processBytes(content, 0, content.length, finalResult, 0)
    finalResult
  }

  def mkPoly(data: Bytes, nonce: Bytes): Bytes = new Bytes(16) match { case output =>
    val polykey = chacha20Run(new Bytes(32), nonce, encrypt = true, skip = false)
    Poly3105.crypto_onetimeauth(output, 0, data, 0, data.length, polykey)
    output
  }

  def encrypt(nonce: Bytes, plainText: Bytes, aad: Bytes) = {
    val cipher = chacha20Run(plainText, nonce, encrypt = true, skip = true)
    val data = jt.concat(aad, jt writeUInt64 aad.length, cipher, jt writeUInt64 cipher.length)
    cipher -> mkPoly(data, nonce)
  }

  def decrypt(mac: Bytes, nonce: Bytes, cipher: Bytes, aad: Bytes) = {
    val check = jt.concat(aad, jt writeUInt64 aad.length, cipher, jt writeUInt64 cipher.length)
    assert(mkPoly(data = check, nonce) sameElements mac, Poly3105.INVALID_MAC)
    chacha20Run(cipher, nonce, encrypt = false, skip = true)
  }
}