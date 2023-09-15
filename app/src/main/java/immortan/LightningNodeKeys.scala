package immortan

import fr.acinq.bitcoin.Crypto.PrivateKey
import fr.acinq.bitcoin.DeterministicWallet._
import immortan.crypto.Tools.Bytes
import scodec.bits.ByteVector


object LightningNodeKeys {
  def makeFromSeed(seed: Bytes): LightningNodeKeys = {
    val master: ExtendedPrivateKey = generate(ByteVector view seed)
    val extendedNodeKey: ExtendedPrivateKey = derivePrivateKey(master, hardened(46L) :: hardened(0L) :: Nil)
    val hashingKey: PrivateKey = derivePrivateKey(master, hardened(138L) :: 0L :: Nil).privateKey
    LightningNodeKeys(master, extendedNodeKey, hashingKey)
  }
}

case class LightningNodeKeys(master: ExtendedPrivateKey, extendedNodeKey: ExtendedPrivateKey, hashingKey: PrivateKey)