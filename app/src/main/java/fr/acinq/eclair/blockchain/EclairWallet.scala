package fr.acinq.eclair.blockchain

import fr.acinq.bitcoin.{ByteVector32, OutPoint, Satoshi, Transaction, TxIn}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import scodec.bits.ByteVector

import scala.concurrent.Future


object EclairWallet {
  final val OPT_IN_FULL_RBF = TxIn.SEQUENCE_FINAL - 2
  final val MAX_RECEIVE_ADDRESSES = 10

  // Wallet types
  final val BIP32 = "BIP32"
  final val BIP44 = "BIP44"
  final val BIP49 = "BIP49"
  final val BIP84 = "BIP84"
}

trait EclairWallet {
  def getData: Future[GetDataResponse]

  def getReceiveAddresses: Future[GetCurrentReceiveAddressesResponse]

  def makeFundingTx(pubkeyScript: ByteVector, amount: Satoshi, feeRatePerKw: FeeratePerKw): Future[GenerateTxResponse]

  def sendPreimageBroadcast(preimages: Set[ByteVector32], pubKeyScript: ByteVector, feeRatePerKw: FeeratePerKw): Future[GenerateTxResponse]

  def makeBatchTx(scriptToAmount: Map[ByteVector, Satoshi], feeRatePerKw: FeeratePerKw): Future[GenerateTxResponse]

  def makeTx(pubKeyScript: ByteVector, amount: Satoshi, prevScriptToAmount: Map[ByteVector, Satoshi], feeRatePerKw: FeeratePerKw): Future[GenerateTxResponse]

  def makeCPFP(fromOutpoints: Set[OutPoint], pubKeyScript: ByteVector, feeRatePerKw: FeeratePerKw): Future[GenerateTxResponse]

  def makeRBFBump(tx: Transaction, feeRatePerKw: FeeratePerKw): Future[RBFResponse]

  def makeRBFReroute(tx: Transaction, feeRatePerKw: FeeratePerKw, pubKeyScript: ByteVector): Future[RBFResponse]

  def provideExcludedOutpoints(excludedOutPoints: List[OutPoint] = Nil): Unit

  def doubleSpent(tx: Transaction): Future[IsDoubleSpentResponse]

  def broadcast(tx: Transaction): Future[Boolean]

  def hasFingerprint: Boolean

  def isBuiltIn: Boolean

  def isSigning: Boolean
}
