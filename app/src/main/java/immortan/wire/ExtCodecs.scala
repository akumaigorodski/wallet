package immortan.wire

import immortan._
import scodec.codecs._
import fr.acinq.eclair.wire.CommonCodecs._
import fr.acinq.eclair.wire.ChannelCodecs._
import fr.acinq.eclair.wire.LightningMessageCodecs._
import fr.acinq.eclair.wire.LastCrossSignedState
import fr.acinq.bitcoin.Crypto.PublicKey


case class HostedState(nodeId1: PublicKey, nodeId2: PublicKey, lastCrossSignedState: LastCrossSignedState)

object ExtCodecs {
  val compressedByteVecCodec = {
    val plain = variableSizeBytes(uint24, bytes)
    zlib(plain)
  }

  val hostedStateCodec = {
    (publicKey withContext "nodeId1") ::
      (publicKey withContext "nodeId2") ::
      (lastCrossSignedStateCodec withContext "lastCrossSignedState")
  }.as[HostedState]

  val lightningNodeKeysCodec = {
    (extendedPrivateKeyCodec withContext "master") ::
    (extendedPrivateKeyCodec withContext "extendedNodeKey") ::
      (privateKey withContext "hashingKey")
  }.as[LightningNodeKeys]

  val walletSecretCodec = {
    (lightningNodeKeysCodec withContext "keys") ::
      (listOfN(uint8, text) withContext "mnemonic") ::
      (varsizebinarydata withContext "seed")
  }.as[WalletSecret]
}
