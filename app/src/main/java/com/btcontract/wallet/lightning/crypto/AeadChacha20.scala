package com.btcontract.wallet.lightning.crypto

import com.btcontract.wallet.lightning.Tools.Bytes
import org.spongycastle.crypto.engines.ChaChaEngine
import com.btcontract.wallet.lightning.Tools.writeUInt64
import org.spongycastle.crypto.params.ParametersWithIV
import org.spongycastle.crypto.params.KeyParameter


class AeadChacha20(key: Bytes, nonce: Bytes) {
  def chacha20Run(content: Bytes, encrypt: Boolean, skip: Boolean) = {
    val cipherParameters = new ParametersWithIV(new KeyParameter(key), nonce)
    val finalResult = new Bytes(content.length)
    val engine = new ChaChaEngine(20)

    engine.init(encrypt, cipherParameters)
    // skip 1 block == set counter to 1 instead of 0
    if (skip) engine.processBytes(new Bytes(64), 0, 64, new Bytes(64), 0)
    engine.processBytes(content, 0, content.length, finalResult, 0)
    finalResult
  }

  def mkPoly(data: Bytes): Bytes = new Bytes(16) match { case output =>
    val polykey = chacha20Run(new Bytes(32), encrypt = true, skip = false)
    Poly3105.crypto_onetimeauth(output, 0, data, 0, data.length, polykey)
    output
  }

  def encrypt(plainText: Bytes, aad: Bytes) = {
    val cipher = chacha20Run(plainText, encrypt = true, skip = true)
    val data = aad ++ writeUInt64(aad.length) ++ cipher ++ writeUInt64(cipher.length)
    cipher -> mkPoly(data)
  }

  def decrypt(cipherText: Bytes, aad: Bytes, mac: Bytes) = {
    val data = aad ++ writeUInt64(aad.length) ++ cipherText ++ writeUInt64(cipherText.length)
    assert(mkPoly(data) sameElements mac, "Invalid Poly1305 MAC detected")
    chacha20Run(cipherText, encrypt = false, skip = true)
  }
}