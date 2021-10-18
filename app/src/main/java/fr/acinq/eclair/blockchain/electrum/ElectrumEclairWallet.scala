package fr.acinq.eclair.blockchain.electrum

import akka.actor.ActorRef
import akka.pattern.ask
import fr.acinq.bitcoin.{ByteVector32, OP_PUSHDATA, OP_RETURN, OutPoint, Satoshi, Script, Transaction, TxIn, TxOut}
import fr.acinq.eclair.addressToPublicKeyScript
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

  import immortan.LNParams.{ec, logBag, timeout}

  type GenerateTxResponseTry = Try[GenerateTxResponse]

  override def getReceiveAddresses: Future[GetCurrentReceiveAddressesResponse] = (walletRef ? GetCurrentReceiveAddresses).mapTo[GetCurrentReceiveAddressesResponse]

  private def isInChain(error: fr.acinq.eclair.blockchain.bitcoind.rpc.Error): Boolean = error.message.toLowerCase.contains("already in block chain")

  private def emptyUtxo(pubKeyScript: ByteVector): TxOut = TxOut(Satoshi(0L), pubKeyScript)

  override def makeFundingTx(pubkeyScript: ByteVector, amount: Satoshi, feeRatePerKw: FeeratePerKw): Future[GenerateTxResponse] = {
    val completeTx = CompleteTransaction(pubkeyScript, amount, feeRatePerKw, sequenceFlag = TxIn.SEQUENCE_FINAL)
    val sendAll = SendAll(pubkeyScript, feeRatePerKw, sequenceFlag = TxIn.SEQUENCE_FINAL)

    (walletRef ? GetBalance).mapTo[GetBalanceResponse] flatMap { chainBalance =>
      val command = if (chainBalance.totalBalance == amount) sendAll else completeTx
      (walletRef ? command).mapTo[GenerateTxResponseTry].map(_.get)
    }
  }

  override def commit(tx: Transaction, tag: String): Future[Boolean] = {
    val broadcast = BroadcastTransaction(tx)
    val commit = CommitTransaction(tx)

    (walletRef ? broadcast).flatMap {
      case ElectrumClient.BroadcastTransactionResponse(_, None) =>
        (walletRef ? commit).mapTo[Boolean]

      case res: ElectrumClient.BroadcastTransactionResponse if res.error.exists(isInChain) =>
        (walletRef ? commit).mapTo[Boolean]

      case res: ElectrumClient.BroadcastTransactionResponse if res.error.isDefined =>
        logBag.put(tag, res.error.get.message)
        Future(false)

      case ElectrumClient.ServerError(_: ElectrumClient.BroadcastTransaction, error) =>
        logBag.put(tag, error.message)
        Future(false)
    }
  }

  override def sendPreimageBroadcast(preimages: Set[ByteVector32], address: String, feeRatePerKw: FeeratePerKw): Future[GenerateTxResponse] = {
    val preimageTxOuts = preimages.toList.map(_.bytes).map(OP_PUSHDATA.apply).grouped(2).map(OP_RETURN :: _).map(Script.write).map(emptyUtxo).toList
    val sendAll = SendAll(Script write addressToPublicKeyScript(address, ewt.chainHash), feeRatePerKw, OPT_IN_FULL_RBF, fromOutpoints = Set.empty, preimageTxOuts)
    (walletRef ? sendAll).mapTo[GenerateTxResponseTry].map(_.get)
  }

  override def makeTx(amount: Satoshi, address: String, feeRatePerKw: FeeratePerKw): Future[GenerateTxResponse] = {
    val sendAll = SendAll(Script write addressToPublicKeyScript(address, ewt.chainHash), feeRatePerKw, OPT_IN_FULL_RBF)
    val completeTx = CompleteTransaction(sendAll.publicKeyScript, amount, feeRatePerKw, OPT_IN_FULL_RBF)

    (walletRef ? GetBalance).mapTo[GetBalanceResponse] flatMap { chainBalance =>
      val command = if (chainBalance.totalBalance == amount) sendAll else completeTx
      (walletRef ? command).mapTo[GenerateTxResponseTry].map(_.get)
    }
  }

  override def makeCPFP(fromOutpoints: Set[OutPoint], address: String, feeRatePerKw: FeeratePerKw): Future[GenerateTxResponse] = {
    val publicKeyScript = Script write addressToPublicKeyScript(address, ewt.chainHash)
    val cpfp = SendAll(publicKeyScript, feeRatePerKw, OPT_IN_FULL_RBF, fromOutpoints)
    (walletRef ? cpfp).mapTo[GenerateTxResponseTry].map(_.get)
  }

  override def makeRBFBump(tx: Transaction, feeRatePerKw: FeeratePerKw): Future[RBFResponse] = {
    val rbfBump = RBFBump(tx, feeRatePerKw, OPT_IN_FULL_RBF)
    (walletRef ? rbfBump).mapTo[RBFResponse]
  }

  override def makeRBFReroute(tx: Transaction, feeRatePerKw: FeeratePerKw, address: String): Future[RBFResponse] = {
    val publicKeyScript = Script write addressToPublicKeyScript(address, ewt.chainHash)
    val rbfReroute = RBFReroute(tx, feeRatePerKw, publicKeyScript, OPT_IN_FULL_RBF)
    (walletRef ? rbfReroute).mapTo[RBFResponse]
  }

  override def doubleSpent(tx: Transaction): Future[DepthAndDoubleSpent] = for {
    response <- (walletRef ? IsDoubleSpent(tx)).mapTo[IsDoubleSpentResponse]
  } yield (response.depth, response.isDoubleSpent)
}
