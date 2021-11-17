package fr.acinq.eclair.blockchain.electrum.db

import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
import fr.acinq.bitcoin.{BlockHeader, ByteVector32, Satoshi}
import fr.acinq.eclair.blockchain.electrum.PersistentData
import scodec.bits.ByteVector


trait HeaderDb {
  type HeightAndHeader = (Int, BlockHeader)
  def addHeaders(headers: Seq[BlockHeader], startHeight: Int): Unit

  def getHeader(height: Int): Option[BlockHeader]
  def getHeader(blockHash: ByteVector32): Option[HeightAndHeader]
  def getHeaders(startHeight: Int, maxCount: Int): Seq[BlockHeader]
  def getTip: Option[HeightAndHeader]
}

sealed trait ChainWalletInfo {
  val masterFingerprint: Option[Long]
  val isRemovable: Boolean
  val walletType: String
}

case class SigningWallet(walletType: String, isRemovable: Boolean) extends ChainWalletInfo { val masterFingerprint: Option[Long] = None }

case class WatchingWallet(walletType: String, masterFingerprint: Option[Long], xPub: ExtendedPublicKey, isRemovable: Boolean) extends ChainWalletInfo

case class CompleteChainWalletInfo(core: ChainWalletInfo, data: ByteVector, lastBalance: Satoshi, label: String, isCoinControlOn: Boolean)

trait WalletDb {
  def remove(pub: PublicKey): Unit
  def addChainWallet(info: CompleteChainWalletInfo, data: ByteVector, pub: PublicKey): Unit
  def persist(data: PersistentData, lastBalance: Satoshi, pub: PublicKey): Unit
  def updateLabel(label: String, pub: PublicKey): Unit
  def listWallets: Iterable[CompleteChainWalletInfo]
}