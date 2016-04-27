package com.btcontract.wallet.helper

import org.bitcoinj.crypto.{EncryptedData, KeyCrypterScrypt}
import org.spongycastle.crypto.params.KeyParameter
import com.btcontract.wallet.Utils.Bytes


object AES { me =>
  val crypter = new KeyCrypterScrypt
  def bytes2Param(raw: Bytes) = new KeyParameter(raw)
  def extract(raw: Bytes) = new EncryptedData(raw take 32, raw drop 32)
  def encrypt(key32: Bytes, plain: Bytes) = crypter.encrypt(plain, me bytes2Param key32)
  def decrypt(key32: Bytes, cipher: EncryptedData) = crypter.decrypt(cipher, me bytes2Param key32)
}