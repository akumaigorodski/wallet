package fr.acinq.eclair.blockchain.electrum

import akka.actor.ActorRef
import akka.pattern.ask
import fr.acinq.bitcoin.{OutPoint, Satoshi, Transaction}
import fr.acinq.eclair.blockchain.EclairWallet
import fr.acinq.eclair.blockchain.EclairWallet._
import fr.acinq.eclair.blockchain.electrum.ElectrumClient.BroadcastTransaction
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._
import fr.acinq.eclair.blockchain.electrum.db.CompleteChainWalletInfo
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import scodec.bits.ByteVector

import scala.concurrent.Future
import scala.util.Try


case class ElectrumEclairWallet(walletRef: ActorRef, ewt: ElectrumWalletType, info: CompleteChainWalletInfo) extends EclairWallet {

  import immortan.WalletParams.{ec, timeout}

  type GenerateTxResponseTry = Try[GenerateTxResponse]

  private def isInChain(error: fr.acinq.eclair.blockchain.bitcoind.rpc.Error): Boolean = error.message.toLowerCase.contains("already in block chain")

  override def getData: Future[GetDataResponse] = (walletRef ? GetData).mapTo[GetDataResponse]

  override def getReceiveAddresses: Future[GetCurrentReceiveAddressesResponse] = (walletRef ? GetCurrentReceiveAddresses).mapTo[GetCurrentReceiveAddressesResponse]

  override def broadcast(tx: Transaction): Future[Boolean] = {
    walletRef ? BroadcastTransaction(tx) flatMap {
      case ElectrumClient.BroadcastTransactionResponse(_, None) => Future(true)
      case res: ElectrumClient.BroadcastTransactionResponse if res.error.exists(isInChain) => Future(true)
      case res: ElectrumClient.BroadcastTransactionResponse if res.error.isDefined => Future(false)
      case ElectrumClient.ServerError(_: ElectrumClient.BroadcastTransaction, _) => Future(false)
    }
  }

  override def makeBatchTx(scriptToAmount: Map[ByteVector, Satoshi], feeRatePerKw: FeeratePerKw): Future[GenerateTxResponse] = {
    val completeTx = CompleteTransaction(scriptToAmount, feeRatePerKw, OPT_IN_FULL_RBF)
    (walletRef ? completeTx).mapTo[GenerateTxResponseTry].map(_.get)
  }

  override def makeTx(pubKeyScript: ByteVector, amount: Satoshi, prevScriptToAmount: Map[ByteVector, Satoshi], feeRatePerKw: FeeratePerKw): Future[GenerateTxResponse] = {
    val sendAll = SendAll(pubKeyScript, prevScriptToAmount, feeRatePerKw, OPT_IN_FULL_RBF, fromOutpoints = Set.empty)

    getData.map(_.data.balance == prevScriptToAmount.values.sum + amount).flatMap {
      case false => makeBatchTx(prevScriptToAmount.updated(pubKeyScript, amount), feeRatePerKw)
      case true => (walletRef ? sendAll).mapTo[GenerateTxResponseTry].map(_.get)
    }
  }

  override def makeCPFP(fromOutpoints: Set[OutPoint], pubKeyScript: ByteVector, feeRatePerKw: FeeratePerKw): Future[GenerateTxResponse] = {
    val cpfp = SendAll(pubKeyScript, pubKeyScriptToAmount = Map.empty, feeRatePerKw, OPT_IN_FULL_RBF, fromOutpoints)
    (walletRef ? cpfp).mapTo[GenerateTxResponseTry].map(_.get)
  }

  override def makeRBFBump(tx: Transaction, feeRatePerKw: FeeratePerKw): Future[RBFResponse] = {
    val rbfBump = RBFBump(tx, feeRatePerKw, OPT_IN_FULL_RBF)
    (walletRef ? rbfBump).mapTo[RBFResponse]
  }

  override def makeRBFReroute(tx: Transaction, feeRatePerKw: FeeratePerKw, pubKeyScript: ByteVector): Future[RBFResponse] = {
    val rbfReroute = RBFReroute(tx, feeRatePerKw, pubKeyScript, OPT_IN_FULL_RBF)
    (walletRef ? rbfReroute).mapTo[RBFResponse]
  }

  override def provideExcludedOutpoints(excludedOutPoints: List[OutPoint] = Nil): Unit = walletRef ! ProvideExcludedOutPoints(excludedOutPoints)

  override def doubleSpent(tx: Transaction): Future[IsDoubleSpentResponse] = (walletRef ? tx).mapTo[IsDoubleSpentResponse]

  override def hasFingerprint: Boolean = info.core.masterFingerprint.nonEmpty

  override def isSigning: Boolean = ewt.secrets.nonEmpty
}
