package fr.acinq.eclair.blockchain

import fr.acinq.bitcoin.{ByteVector32, OutPoint, Satoshi, Transaction, TxIn}
import fr.acinq.eclair.blockchain.EclairWallet._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{GenerateTxResponse, GetCurrentReceiveAddressesResponse, RBFResponse}
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import scodec.bits.ByteVector

import scala.concurrent.Future


object EclairWallet {
  type DepthAndDoubleSpent = (Long, Boolean)
  final val OPT_IN_FULL_RBF = TxIn.SEQUENCE_FINAL - 2
  final val MAX_RECEIVE_ADDRESSES = 10

  // Wallet types
  final val BIP32 = "BIP32"
  final val BIP44 = "BIP44"
  final val BIP49 = "BIP49"
  final val BIP84 = "BIP84"
}

trait EclairWallet {
  def getReceiveAddresses: Future[GetCurrentReceiveAddressesResponse]

  def makeFundingTx(pubkeyScript: ByteVector, amount: Satoshi, feeRatePerKw: FeeratePerKw): Future[GenerateTxResponse]

  def sendPreimageBroadcast(preimages: Set[ByteVector32], address: String, feeRatePerKw: FeeratePerKw): Future[GenerateTxResponse]

  def makeTx(amount: Satoshi, address: String, feeRatePerKw: FeeratePerKw): Future[GenerateTxResponse]

  def makeCPFP(fromOutpoints: Set[OutPoint], address: String, feeRatePerKw: FeeratePerKw): Future[GenerateTxResponse]

  def makeRBFBump(tx: Transaction, feeRatePerKw: FeeratePerKw): Future[RBFResponse]

  def makeRBFReroute(tx: Transaction, feeRatePerKw: FeeratePerKw, address: String): Future[RBFResponse]

  def commit(tx: Transaction, tag: String): Future[Boolean]

  def doubleSpent(tx: Transaction): Future[DepthAndDoubleSpent]
}
