package com.btcontract.wallet.lightning

import org.bitcoinj.core.Sha256Hash
import org.bitcoinj.core.Utils.HEX


object Tools {
  type Bytes = Array[Byte]

  val strToBytes = (_: String) getBytes "UTF-8"
  val bytesToJson = new String(_: Bytes, "UTF-8")

  def hash2256Hex(raw: Bytes) = HEX.encode(Sha256Hash hashTwice raw)
}
