package fr.acinq.eclair.blockchain

import fr.acinq.eclair.blockchain.EclairWallet._
import fr.acinq.bitcoin.{ByteVector32, OutPoint, Satoshi, Transaction, TxIn}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.GetCurrentReceiveAddressesResponse
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import scala.concurrent.Future
import scodec.bits.ByteVector


object EclairWallet {
  type DepthAndDoubleSpent = (Long, Boolean)
  final val OPT_IN_FULL_RBF = TxIn.SEQUENCE_FINAL - 2
  final val MAX_RECEIVE_ADDRESSES = 10

  final val BIP32 = "BIP32"
  final val BIP44 = "BIP44"
  final val BIP49 = "BIP49"
  final val BIP84 = "BIP84"
}

trait EclairWallet {
  def getReceiveAddresses: Future[GetCurrentReceiveAddressesResponse]

  def makeFundingTx(pubkeyScript: ByteVector, amount: Satoshi, feeRatePerKw: FeeratePerKw): Future[MakeFundingTxResponse]

  def sendPreimageBroadcast(preimages: Set[ByteVector32], address: String, feeRatePerKw: FeeratePerKw): Future[TxAndFee]

  def makeTx(amount: Satoshi, address: String, feeRatePerKw: FeeratePerKw): Future[TxAndFee]

  def makeCPFP(fromOutpoints: Set[OutPoint], address: String, feeRatePerKw: FeeratePerKw): Future[TxAndFee]

  def commit(tx: Transaction, tag: String): Future[Boolean]

  def doubleSpent(tx: Transaction): Future[DepthAndDoubleSpent]
}

case class TxAndFee(tx: Transaction, fee: Satoshi)

case class MakeFundingTxResponse(fundingTx: Transaction, fundingTxOutputIndex: Int, fee: Satoshi) {
  val fundingPubkeyScript: ByteVector = fundingTx.txOut(fundingTxOutputIndex).publicKeyScript
  val fundingAmount: Satoshi = fundingTx.txOut(fundingTxOutputIndex).amount
}
