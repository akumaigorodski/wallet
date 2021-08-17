package immortan

import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.{ByteVector32, Protocol}
import fr.acinq.eclair.crypto.Mac32
import java.io.ByteArrayInputStream
import immortan.crypto.Tools.Bytes
import scodec.bits.ByteVector
import java.nio.ByteOrder


object LightningNodeKeys {
  def makeFromSeed(seed: Bytes): LightningNodeKeys = {
    val master: ExtendedPrivateKey = generate(ByteVector view seed)
    val extendedNodeKey: ExtendedPrivateKey = derivePrivateKey(master, hardened(46L) :: hardened(0L) :: Nil)
    val hashingKey: PrivateKey = derivePrivateKey(master, hardened(138L) :: 0L :: Nil).privateKey
    LightningNodeKeys(master, extendedNodeKey, hashingKey)
  }
}

case class LightningNodeKeys(master: ExtendedPrivateKey, extendedNodeKey: ExtendedPrivateKey, hashingKey: PrivateKey) {
  lazy val ourNodePrivateKey: PrivateKey = extendedNodeKey.privateKey

  def makeLinkingKey(domain: String): PrivateKey = {
    val domainBytes = ByteVector(domain getBytes "UTF-8")
    val wifHashingKeyBytes = hashingKey.value.bytes :+ 1.toByte
    val pathMaterial = Mac32.hmac256(wifHashingKeyBytes, domainBytes)
    val chain = hardened(138) :: makeKeyPath(pathMaterial.bytes)
    // use master here to be compatible with old BLW
    derivePrivateKey(master, chain).privateKey
  }

  def fakeInvoiceKey(paymentHash: ByteVector32): PrivateKey = {
    val chain = hardened(184) :: makeKeyPath(paymentHash.bytes)
    derivePrivateKey(extendedNodeKey, chain).privateKey
  }

  def ourFakeNodeIdKey(theirNodeId: PublicKey): ExtendedPrivateKey = {
    val chain = hardened(230) :: makeKeyPath(theirNodeId.value)
    derivePrivateKey(extendedNodeKey, chain)
  }

  private def makeKeyPath(material: ByteVector): List[Long] = {
    require(material.size > 15, "Material size must be at least 16")
    val stream = new ByteArrayInputStream(material.slice(0, 16).toArray)
    def getChunk = Protocol.uint32(stream, ByteOrder.BIG_ENDIAN)
    List.fill(4)(getChunk)
  }
}