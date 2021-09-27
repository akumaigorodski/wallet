package fr.acinq.eclair.blockchain.electrum

import fr.acinq.eclair.blockchain.EclairWallet._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._
import fr.acinq.eclair.blockchain.{EclairWallet, MakeFundingTxResponse, TxAndFee}
import fr.acinq.bitcoin.{ByteVector32, OP_PUSHDATA, OP_RETURN, OutPoint, Satoshi, Script, Transaction, TxIn, TxOut}
import fr.acinq.eclair.blockchain.electrum.ElectrumClient.BroadcastTransaction
import fr.acinq.eclair.blockchain.electrum.db.CompleteChainWalletInfo
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.addressToPublicKeyScript

import scala.concurrent.Future
import scodec.bits.ByteVector
import akka.actor.ActorRef
import akka.pattern.ask


case class ElectrumEclairWallet(walletRef: ActorRef, ewt: ElectrumWalletType, info: CompleteChainWalletInfo) extends EclairWallet {

  import immortan.LNParams.{ec, timeout, logBag}

  override def getReceiveAddresses: Future[GetCurrentReceiveAddressesResponse] = (walletRef ? GetCurrentReceiveAddresses).mapTo[GetCurrentReceiveAddressesResponse]

  private def isInChain(error: fr.acinq.eclair.blockchain.bitcoind.rpc.Error): Boolean = error.message.toLowerCase.contains("already in block chain")

  private def emptyUtxo(pubKeyScript: ByteVector): TxOut = TxOut(Satoshi(0L), pubKeyScript)

  override def makeFundingTx(pubkeyScript: ByteVector, amount: Satoshi, feeRatePerKw: FeeratePerKw): Future[MakeFundingTxResponse] =
    (walletRef ? GetBalance).mapTo[GetBalanceResponse].flatMap {
      case chainBalance if chainBalance.totalBalance == amount =>
        val sendAllCommand = SendAll(pubkeyScript, feeRatePerKw, TxIn.SEQUENCE_FINAL)
        (walletRef ? sendAllCommand).mapTo[SendAllResponse].map(_.result).map {
          case Some(res) => MakeFundingTxResponse(res.tx, 0, res.fee)
          case None => throw new RuntimeException
        }

      case _ =>
        val txOut = TxOut(amount, pubkeyScript)
        val tx = Transaction(version = 2, txIn = Nil, txOut = txOut :: Nil, lockTime = 0)
        val completeTxCommand = CompleteTransaction(tx, feeRatePerKw, TxIn.SEQUENCE_FINAL)
        (walletRef ? completeTxCommand).mapTo[CompleteTransactionResponse].map(_.result).map {
          case Some(res) => MakeFundingTxResponse(res.tx, 0, res.fee)
          case None => throw new RuntimeException
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

  override def sendPreimageBroadcast(preimages: Set[ByteVector32], address: String, feeRatePerKw: FeeratePerKw): Future[TxAndFee] = {
    val preimageTxOuts = preimages.toList.map(_.bytes).map(OP_PUSHDATA.apply).grouped(2).map(OP_RETURN :: _).map(Script.write).map(emptyUtxo).toList
    val sendAll = SendAll(Script write addressToPublicKeyScript(address, ewt.chainHash), feeRatePerKw, OPT_IN_FULL_RBF, fromOutpoints = Set.empty, preimageTxOuts)
    (walletRef ? sendAll).mapTo[SendAllResponse].map(_.result.get)
  }

  override def makeTx(amount: Satoshi, address: String, feeRatePerKw: FeeratePerKw): Future[TxAndFee] = {
    val publicKeyScript = Script write addressToPublicKeyScript(address, ewt.chainHash)

    (walletRef ? GetBalance).mapTo[GetBalanceResponse].flatMap {
      case chainBalance if chainBalance.totalBalance == amount =>
        val sendAll = SendAll(publicKeyScript, feeRatePerKw, OPT_IN_FULL_RBF)
        (walletRef ? sendAll).mapTo[SendAllResponse].map(_.result.get)

      case _ =>
        val txOut = TxOut(amount, publicKeyScript)
        val tx = Transaction(version = 2, txIn = Nil, txOut = txOut :: Nil, lockTime = 0)
        val completeTx = CompleteTransaction(tx, feeRatePerKw, sequenceFlag = OPT_IN_FULL_RBF)
        (walletRef ? completeTx).mapTo[CompleteTransactionResponse].map(_.result.get)
    }
  }

  override def makeCPFP(fromOutpoints: Set[OutPoint], address: String, feeRatePerKw: FeeratePerKw): Future[TxAndFee] = {
    val publicKeyScript = Script write addressToPublicKeyScript(address, ewt.chainHash)
    val cpfp = SendAll(publicKeyScript, feeRatePerKw, OPT_IN_FULL_RBF, fromOutpoints)
    (walletRef ? cpfp).mapTo[SendAllResponse].map(_.result.get)
  }

  override def doubleSpent(tx: Transaction): Future[DepthAndDoubleSpent] = for {
    response <- (walletRef ? IsDoubleSpent(tx)).mapTo[IsDoubleSpentResponse]
  } yield (response.depth, response.isDoubleSpent)
}
