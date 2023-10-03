package immortan

import java.io.ByteArrayInputStream
import java.nio.ByteOrder

import fr.acinq.bitcoin.Crypto.PrivateKey
import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.bitcoin.Protocol
import fr.acinq.eclair.crypto.Mac32
import immortan.crypto.Tools.{Bytes, StringList}
import scodec.bits.ByteVector


object LightningNodeKeys {
  def fromSeed(seed: Bytes): LightningNodeKeys = {
    val master = generate(ByteVector view seed)
    fromMaster(master)
  }

  def fromMaster(master: ExtendedPrivateKey): LightningNodeKeys = {
    val extendedNodeKey: ExtendedPrivateKey = derivePrivateKey(master, hardened(46L) :: hardened(0L) :: Nil)
    val hashingKey: PrivateKey = derivePrivateKey(master, hardened(138L) :: 0L :: Nil).privateKey
    LightningNodeKeys(master, extendedNodeKey, hashingKey)
  }
}

case class WalletSecret(keys: LightningNodeKeys, mnemonic: StringList, seed: ByteVector)
case class LightningNodeKeys(master: ExtendedPrivateKey, extendedNodeKey: ExtendedPrivateKey, hashingKey: PrivateKey) {

  def makeLinkingKey(domain: String): PrivateKey = {
    val domainBytes = ByteVector(domain getBytes "UTF-8")
    val wifHashingKeyBytes = hashingKey.value.bytes :+ 1.toByte
    val pathMaterial = Mac32.hmac256(wifHashingKeyBytes, domainBytes)
    val chain = hardened(138) :: makeKeyPath(pathMaterial.bytes)
    derivePrivateKey(extendedNodeKey, chain).privateKey
  }

  private def makeKeyPath(material: ByteVector): List[Long] = {
    require(material.size > 15, "Material size must be at least 16")
    val stream = new ByteArrayInputStream(material.slice(0, 16).toArray)
    def getChunk = Protocol.uint32(stream, ByteOrder.BIG_ENDIAN)
    List.fill(4)(getChunk)
  }
}
