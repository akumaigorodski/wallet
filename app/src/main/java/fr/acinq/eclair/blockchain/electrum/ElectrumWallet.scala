package fr.acinq.eclair.blockchain.electrum

import fr.acinq.bitcoin._
import immortan.crypto.Tools._
import scala.concurrent.duration._
import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.eclair.blockchain.EclairWallet._
import fr.acinq.eclair.blockchain.electrum.ElectrumClient._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._

import scala.util.{Success, Try}
import akka.actor.{ActorRef, FSM, PoisonPill}
import fr.acinq.eclair.blockchain.electrum.db.{HeaderDb, WalletDb}
import fr.acinq.eclair.blockchain.electrum.db.sqlite.SqliteWalletDb.persistentDataCodec
import fr.acinq.eclair.blockchain.bitcoind.rpc.Error
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.transactions.Transactions
import fr.acinq.eclair.blockchain.TxAndFee
import Blockchain.RETARGETING_PERIOD
import scala.collection.immutable
import scala.annotation.tailrec
import scodec.bits.ByteVector


class ElectrumWallet(client: ActorRef, chainSync: ActorRef, params: WalletParameters, ewt: ElectrumWalletType) extends FSM[State, ElectrumData] {

  def persistAndNotify(data: ElectrumData): ElectrumData = {
    setTimer(KEY_REFILL, KEY_REFILL, 200.millis, repeat = false)
    if (data.lastReadyMessage contains data.currentReadyMessage) return data
    val data1 = data.copy(lastReadyMessage = data.currentReadyMessage.asSome)
    params.walletDb.persist(data1.toPersistent, data1.balance.totalBalance, ewt.xPub.publicKey)
    context.system.eventStream.publish(data1.currentReadyMessage)
    data1
  }

  context.system.eventStream.subscribe(channel = classOf[Blockchain], subscriber = self)

  client ! ElectrumClient.AddStatusListener(self)

  startWith(DISCONNECTED, null)

  when(DISCONNECTED) {
    case Event(raw: ByteVector, null) =>
      // Serialized data may become big with much usage
      // Deserialzie it in this dedicated thread to not slow down UI

      val persisted: PersistentData = persistentDataCodec.decode(raw.toBitVector).require.value
      val firstAccountKeys = for (idx <- math.max(persisted.accountKeysCount - 1000, 0) until persisted.accountKeysCount) yield derivePublicKey(ewt.accountMaster, idx)
      val firstChangeKeys = for (idx <- math.max(persisted.changeKeysCount - 1000, 0) until persisted.changeKeysCount) yield derivePublicKey(ewt.changeMaster, idx)

      val blockchain0 = Blockchain(ewt.chainHash, checkpoints = Vector.empty, headersMap = Map.empty, bestchain = Vector.empty)
      val data0 = ElectrumData(ewt, blockchain0, firstAccountKeys.toVector, firstChangeKeys.toVector, persisted.status, persisted.transactions,
        persisted.history, persisted.proofs, pendingHistoryRequests = Set.empty, pendingHeadersRequests = Set.empty, pendingTransactionRequests = Set.empty,
        pendingTransactions = persisted.pendingTransactions)
      stay using data0

    case Event(blockchain1: Blockchain, data) =>
      for (key <- data.accountKeys) client ! ElectrumClient.ScriptHashSubscription(ewt.computeScriptHashFromPublicKey(key.publicKey), self)
      for (key <- data.changeKeys) client ! ElectrumClient.ScriptHashSubscription(ewt.computeScriptHashFromPublicKey(key.publicKey), self)
      val data1 = data.copy(blockchain = blockchain1)
      goto(RUNNING) using persistAndNotify(data1)
  }

  when(RUNNING) {
    case Event(blockchain1: Blockchain, data) =>
      val data1 = data.copy(blockchain = blockchain1)
      data1.pendingMerkleResponses.foreach(self.!)
      stay using persistAndNotify(data1)

    case Event(ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, status), data) if data.status.get(scriptHash).contains(status) =>
      val missing = data.history.getOrElse(scriptHash, Nil).map(item => item.txHash -> item.height).toMap -- data.transactions.keySet -- data.pendingTransactionRequests

      missing.foreach { case (txid, height) =>
        client ! GetTransaction(txid, contextOpt = None)
        client ! GetMerkle(txid, height)
      }

      if (missing.nonEmpty) {
        // An optimization to not recalculate internal data values on each scriptHashResponse event
        val data1 = data.copy(pendingHistoryRequests = data.pendingTransactionRequests ++ missing.keySet)
        stay using persistAndNotify(data1)
      } else stay

    case Event(ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, _), data) if !data.accountKeyMap.contains(scriptHash) && !data.changeKeyMap.contains(scriptHash) => stay

    case Event(ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, status), data) if status.isEmpty =>
      val status1 = data.status.updated(scriptHash, status)
      val data1 = data.copy(status = status1)
      stay using persistAndNotify(data1)

    case Event(ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, status), data) =>
      val data1 = data.copy(status = data.status.updated(scriptHash, status), pendingHistoryRequests = data.pendingHistoryRequests + scriptHash)
      client ! ElectrumClient.GetScriptHashHistory(scriptHash)
      stay using persistAndNotify(data1)

    case Event(ElectrumClient.GetScriptHashHistoryResponse(scriptHash, items), data) =>
      val pendingHeadersRequests1 = collection.mutable.HashSet.empty[GetHeaders]
      pendingHeadersRequests1 ++= data.pendingHeadersRequests

      val shadowItems = for {
        existingItems <- data.history.get(scriptHash).toList
        item <- existingItems if !items.exists(_.txHash == item.txHash)
      } yield item

      val items1 = items ++ shadowItems

      def downloadHeadersIfMissing(height: Int): Unit = {
        if (data.blockchain.getHeader(height).orElse(params.headerDb getHeader height).isEmpty) {
          // we don't have this header because it is older than our checkpoints => request the entire chunk
          val request = GetHeaders(height / RETARGETING_PERIOD * RETARGETING_PERIOD, RETARGETING_PERIOD)
          if (pendingHeadersRequests1 contains request) return
          pendingHeadersRequests1.add(request)
          chainSync ! request
        }
      }

      def process(txid: ByteVector32, height: Int): Unit = {
        if (data.proofs.contains(txid) || height <= 0) return
        downloadHeadersIfMissing(height)
        client ! GetMerkle(txid, height)
      }

      val pendingTransactionRequests1 = items1.foldLeft(data.pendingTransactionRequests) {
        case (hashes, item) if !data.transactions.contains(item.txHash) && !data.pendingTransactionRequests.contains(item.txHash) =>
          client ! GetTransaction(item.txHash)
          process(item.txHash, item.height)
          hashes + item.txHash

        case (hashes, item) =>
          process(item.txHash, item.height)
          hashes
      }

      val data1 = data.copy(history = data.history.updated(scriptHash, items1),
        pendingHistoryRequests = data.pendingHistoryRequests - scriptHash,
        pendingTransactionRequests = pendingTransactionRequests1,
        pendingHeadersRequests = pendingHeadersRequests1.toSet)
      stay using persistAndNotify(data1)

    case Event(GetTransactionResponse(tx, contextOpt), data) =>
      val data1 = data.copy(pendingTransactionRequests = data.pendingTransactionRequests - tx.txid)

      data.computeTransactionDelta(tx) map { case (received, sent, feeOpt) =>
        for (pendingTx <- data.pendingTransactions) self ! GetTransactionResponse(pendingTx, contextOpt)
        context.system.eventStream publish data.transactionReceived(tx, feeOpt, received, sent, ewt.xPub)
        val data2 = data1.copy(transactions = data.transactions.updated(tx.txid, tx), pendingTransactions = Nil)
        stay using persistAndNotify(data2)
      } getOrElse {
        // We are currently missing parents for this transaction
        val data2 = data1.copy(pendingTransactions = data.pendingTransactions :+ tx)
        stay using persistAndNotify(data2)
      }

    case Event(ServerError(GetTransaction(txid, _), error), data) if data.pendingTransactionRequests.contains(txid) =>
      log.error(s"Electrum server cannot find history for txid $txid with error $error")
      goto(DISCONNECTED) replying PoisonPill

    case Event(response @ GetMerkleResponse(txid, _, height, _, _), data) =>
      val request = GetHeaders(height / RETARGETING_PERIOD * RETARGETING_PERIOD, RETARGETING_PERIOD)

      data.blockchain.getHeader(height) orElse params.headerDb.getHeader(height) match {
        case Some(foundHeader) if foundHeader.hashMerkleRoot == response.root && data.isTxKnown(txid) =>
          val data1 = data.copy(proofs = data.proofs.updated(txid, response), pendingMerkleResponses = data.pendingMerkleResponses - response)
          stay using persistAndNotify(data1)

        case Some(foundHeader) if foundHeader.hashMerkleRoot == response.root => stay

        case None if data.pendingHeadersRequests.contains(request) =>
          stay using data.copy(pendingMerkleResponses = data.pendingMerkleResponses + response)

        case None =>
          chainSync ! request
          val data1 = data.copy(pendingHeadersRequests = data.pendingHeadersRequests + request)
          stay using data1.copy(pendingMerkleResponses = data1.pendingMerkleResponses + response)

        case _ =>
          val data1 = data.copy(transactions = data.transactions - txid)
          stay using data1 replying PoisonPill
      }

    case Event(bc: ElectrumClient.BroadcastTransaction, _) =>
      client forward bc
      stay

    case Event(commit: CommitTransaction, data) =>
      val data1 = data.commitTransaction(commit.tx)
      // We use the initial state to compute the effect of the tx
      val (received, sent, fee) = data.computeTransactionDelta(commit.tx).get
      // We notify here because the tx won't be downloaded again (it has been added to the state at commit)
      context.system.eventStream publish data1.transactionReceived(commit.tx, fee, received, sent, ewt.xPub)
      stay using persistAndNotify(data1) replying true
  }

  whenUnhandled {
    case Event(IsDoubleSpent(tx), data) =>
      val txOutPoints = tx.txIn.map(_.outPoint).toSet
      val doubleSpendTrials: immutable.Iterable[Boolean] = for {
        (txid, proof) <- data.proofs if data.computeDepth(proof.blockHeight) > 1
        spendingTx <- data.transactions.get(txid) if spendingTx.txid != tx.txid
        spendOutPoints = spendingTx.txIn.map(_.outPoint).toSet
      } yield spendOutPoints.intersect(txOutPoints).nonEmpty

      val depth = data.computeTransactionDepth(tx.txid)
      val isDoubleSpent = doubleSpendTrials.exists(identity)
      stay replying IsDoubleSpentResponse(tx, depth, isDoubleSpent)

    case Event(GetCurrentReceiveAddresses, data) => stay replying GetCurrentReceiveAddressesResponse(data.currentReceiveAddresses)

    case Event(ElectrumClient.ElectrumDisconnected, data) => goto(DISCONNECTED) using data.reset

    case Event(GetBalance, data) => stay replying data.balance

    case Event(CompleteTransaction(tx, feeRatePerKw, sequenceFlag), data) =>
      Try apply data.completeTransaction(tx, feeRatePerKw, params.dustLimit, params.allowSpendUnconfirmed, sequenceFlag) match {
        case Success(txAndFee) => stay replying CompleteTransactionResponse(txAndFee.asSome)
        case _ => stay replying CompleteTransactionResponse(None)
      }

    case Event(SendAll(publicKeyScript, extraUtxos, feeRatePerKw, sequenceFlag), data) =>
      Try apply data.spendAll(publicKeyScript, extraUtxos, feeRatePerKw, params.dustLimit, sequenceFlag) match {
        case Success(txAndFee) => stay replying SendAllResponse(txAndFee.asSome)
        case _ => stay replying SendAllResponse(None)
      }

    case Event(ElectrumClient.BroadcastTransaction(tx), _) =>
      val notConnected = Error(code = -1, "wallet is not connected").asSome
      stay replying ElectrumClient.BroadcastTransactionResponse(tx, notConnected)

    case Event(KEY_REFILL, data) if data.firstUnusedChangeKey.isEmpty =>
      val newKey = derivePublicKey(ewt.changeMaster, data.changeKeys.last.path.lastChildNumber + 1)
      val newKeyScriptHash = ewt.computeScriptHashFromPublicKey(newKey.publicKey)
      client ! ElectrumClient.ScriptHashSubscription(newKeyScriptHash, self)

      val changeKeys1 = data.changeKeys :+ newKey
      val status1 = data.status.updated(newKeyScriptHash, new String)
      val data1 = data.copy(status = status1, changeKeys = changeKeys1)
      stay using persistAndNotify(data1)

    case Event(KEY_REFILL, data) if data.firstUnusedAccountKeys.size < MAX_RECEIVE_ADDRESSES =>
      val newKey = derivePublicKey(ewt.accountMaster, data.accountKeys.last.path.lastChildNumber + 1)
      val newKeyScriptHash = ewt.computeScriptHashFromPublicKey(newKey.publicKey)
      client ! ElectrumClient.ScriptHashSubscription(newKeyScriptHash, self)

      val accountKeys1 = data.accountKeys :+ newKey
      val status1 = data.status.updated(newKeyScriptHash, new String)
      val data1 = data.copy(status = status1, accountKeys = accountKeys1)
      stay using persistAndNotify(data1)
  }

  initialize
}

object ElectrumWallet {
  type TransactionHistoryItemList = List[ElectrumClient.TransactionHistoryItem]

  final val KEY_REFILL = "key-refill"

  sealed trait State
  case object DISCONNECTED extends State
  case object WAITING_FOR_TIP extends State
  case object SYNCING extends State
  case object RUNNING extends State

  sealed trait Request
  sealed trait Response

  case object GetBalance extends Request
  case class GetBalanceResponse(totalBalance: Satoshi) extends Response

  case object GetCurrentReceiveAddresses extends Request
  case class GetCurrentReceiveAddressesResponse(address2PubKey: Map[String, ExtendedPublicKey] = Map.empty) extends Response

  case class CompleteTransaction(tx: Transaction, feeRatePerKw: FeeratePerKw, sequenceFlag: Long) extends Request
  case class CompleteTransactionResponse(result: Option[TxAndFee] = None) extends Response

  case class SendAll(publicKeyScript: ByteVector, extraUtxos: List[TxOut], feeRatePerKw: FeeratePerKw, sequenceFlag: Long) extends Request
  case class SendAllResponse(result: Option[TxAndFee] = None) extends Response

  case class ChainFor(target: ActorRef) extends Request

  case class CommitTransaction(tx: Transaction) extends Request

  case class IsDoubleSpent(tx: Transaction) extends Request
  case class IsDoubleSpentResponse(tx: Transaction, depth: Long, isDoubleSpent: Boolean) extends Response

  sealed trait WalletEvent { val xPub: ExtendedPublicKey }
  case class TransactionReceived(tx: Transaction, depth: Long, received: Satoshi, sent: Satoshi, walletAddreses: List[String], xPub: ExtendedPublicKey, feeOpt: Option[Satoshi] = None) extends WalletEvent
  case class WalletReady(balance: Satoshi, height: Long, heightsCode: Int, xPub: ExtendedPublicKey) extends WalletEvent
}

case class Utxo(key: ExtendedPublicKey, item: ElectrumClient.UnspentItem)

case class AccountAndXPrivKey(xPriv: ExtendedPrivateKey, master: ExtendedPrivateKey)

case class WalletParameters(headerDb: HeaderDb, walletDb: WalletDb, dustLimit: Satoshi, allowSpendUnconfirmed: Boolean) {
  lazy val emptyPersistentData: PersistentData = PersistentData(accountKeysCount = MAX_RECEIVE_ADDRESSES, changeKeysCount = MAX_RECEIVE_ADDRESSES)
  lazy val emptyPersistentDataBytes: ByteVector = persistentDataCodec.encode(emptyPersistentData).require.toByteVector
}

case class ElectrumData(ewt: ElectrumWalletType, blockchain: Blockchain, accountKeys: Vector[ExtendedPublicKey], changeKeys: Vector[ExtendedPublicKey], status: Map[ByteVector32, String] = Map.empty,
                        transactions: Map[ByteVector32, Transaction] = Map.empty, history: Map[ByteVector32, TransactionHistoryItemList] = Map.empty, proofs: Map[ByteVector32, GetMerkleResponse] = Map.empty,
                        pendingHistoryRequests: Set[ByteVector32] = Set.empty, pendingTransactionRequests: Set[ByteVector32] = Set.empty, pendingHeadersRequests: Set[GetHeaders] = Set.empty,
                        pendingTransactions: List[Transaction] = Nil, pendingMerkleResponses: Set[GetMerkleResponse] = Set.empty, lastReadyMessage: Option[WalletReady] = None) {

  lazy val accountKeyMap: Map[ByteVector32, ExtendedPublicKey] = accountKeys.map(key => ewt.computeScriptHashFromPublicKey(key.publicKey) -> key).toMap

  lazy val changeKeyMap: Map[ByteVector32, ExtendedPublicKey] = changeKeys.map(key => ewt.computeScriptHashFromPublicKey(key.publicKey) -> key).toMap

  lazy val currentReadyMessage: WalletReady = WalletReady(balance.totalBalance, blockchain.tip.height, proofs.hashCode, ewt.xPub)

  lazy val firstUnusedAccountKeys: immutable.Iterable[ExtendedPublicKey] = accountKeyMap.collect { case Tuple2(scriptHash, privKey) if status.get(scriptHash).contains(new String) => privKey }

  lazy val firstUnusedChangeKey: Option[ExtendedPublicKey] = changeKeyMap.collectFirst { case Tuple2(scriptHash, privKey) if status.get(scriptHash).contains(new String) => privKey }

  lazy val publicScriptMap: Map[ByteVector, ExtendedPublicKey] = (accountKeys ++ changeKeys).map(key => Script.write(ewt computePublicKeyScript key.publicKey) -> key).toMap

  lazy val utxos: Seq[Utxo] = history.keys.toList.flatMap(getUtxos)

  def currentReceiveAddresses: Map[String, ExtendedPublicKey] = firstUnusedAccountKeys match {
    case keys if keys.isEmpty => accountKeys.map(ewt.textAddress).zip(accountKeys).toMap
    case keys => keys.map(ewt.textAddress).zip(keys).toMap
  }

  // Remove status for each script hash for which we have pending requests, this will make us query script hash history for these script hashes again when we reconnect
  def reset: ElectrumData = copy(status = status -- pendingHistoryRequests, pendingHistoryRequests = Set.empty, pendingTransactionRequests = Set.empty, pendingHeadersRequests = Set.empty, lastReadyMessage = None)

  def isTxKnown(txid: ByteVector32): Boolean = transactions.contains(txid) || pendingTransactionRequests.contains(txid) || pendingTransactions.exists(_.txid == txid)

  def isMine(txIn: TxIn): Boolean = ewt.extractPubKeySpentFrom(txIn).map(ewt.computePublicKeyScript).map(Script.write).exists(publicScriptMap.contains)

  def isReceive(txOut: TxOut, scriptHash: ByteVector32): Boolean = publicScriptMap.get(txOut.publicKeyScript).exists(key => ewt.computeScriptHashFromPublicKey(key.publicKey) == scriptHash)

  def accountOrChangeKey(scriptHash: ByteVector32): ExtendedPublicKey = accountKeyMap.get(scriptHash) match { case None => changeKeyMap(scriptHash) case Some(key) => key }

  def toPersistent: PersistentData = PersistentData(accountKeys.length, changeKeys.length, status, transactions, Map.empty, history, proofs, pendingTransactions)

  def computeTransactionDepth(txid: ByteVector32): Int = proofs.get(txid).map(_.blockHeight).map(computeDepth).getOrElse(0)

  def computeDepth(txHeight: Int): Int = if (txHeight <= 0L) 0 else blockchain.height - txHeight + 1

  def isMine(txOut: TxOut): Boolean = publicScriptMap.contains(txOut.publicKeyScript)

  def getUtxos(scriptHash: ByteVector32): Seq[Utxo] =
    history.get(scriptHash) match {
      case Some(Nil) => Nil
      case None => Nil

      case Some(historyItems) =>
        accountKeyMap.get(scriptHash) orElse changeKeyMap.get(scriptHash) map { key =>
          // We definitely have a private key generated for corresponding scriptHash here

          val unspents = for {
            item <- historyItems
            tx <- transactions.get(item.txHash).toList
            (txOut, index) <- tx.txOut.zipWithIndex if isReceive(txOut, scriptHash)
            unspent = ElectrumClient.UnspentItem(item.txHash, index, txOut.amount.toLong, item.height)
          } yield Utxo(key, unspent)

          // Find all transactions that send to or receive from this script hash
          val txs = historyItems.flatMap(transactions get _.txHash).flatMap(_.txIn).map(_.outPoint)
          // Because we may have unconfirmed UTXOs that are spend by unconfirmed transactions
          unspents.filterNot(utxo => txs contains utxo.item.outPoint)
        } getOrElse Nil
    }

  lazy val balance: GetBalanceResponse = GetBalanceResponse(utxos.map(_.item.value.sat).sum)

  def transactionReceived(tx: Transaction, feeOpt: Option[Satoshi], received: Satoshi, sent: Satoshi, xPub: ExtendedPublicKey): TransactionReceived = {
    val walletAddresses = tx.txOut.filter(isMine).map(_.publicKeyScript).flatMap(publicScriptMap.get).map(ewt.textAddress).toList
    TransactionReceived(tx, computeTransactionDepth(tx.txid), received, sent, walletAddresses, xPub, feeOpt)
  }

  type SatOpt = Option[Satoshi]

  type ReceivedSentFee = (Satoshi, Satoshi, SatOpt)

  def computeTransactionDelta(tx: Transaction): Option[ReceivedSentFee] = {
    // Computes the effect of this transaction on the wallet
    val ourInputs = tx.txIn.filter(isMine)

    val missingParent = ourInputs.exists { txIn =>
      !transactions.contains(txIn.outPoint.txid)
    }

    if (missingParent) None else {
      val received = tx.txOut.filter(isMine).map(_.amount).sum
      val sent = ourInputs.map(txIn => transactions(txIn.outPoint.txid) txOut txIn.outPoint.index.toInt).map(_.amount).sum
      val feeOpt = if (ourInputs.size == tx.txIn.size) Some(sent - tx.txOut.map(_.amount).sum) else None
      (received, sent, feeOpt).asSome
    }
  }

  def completeTransaction(tx: Transaction, feeRatePerKw: FeeratePerKw, dustLimit: Satoshi, allowSpendUnconfirmed: Boolean, sequenceFlag: Long): TxAndFee = {
    val usable = if (allowSpendUnconfirmed) utxos.sortBy(_.item.value) else utxos.filter(_.item.height > 0).sortBy(_.item.value)
    val amount = tx.txOut.map(_.amount).sum

    def computeFee(candidates: Seq[Utxo], change: Option[TxOut] = None): Satoshi = {
      val tx1 = ewt.addUtxosWithDummySig(usableUtxos = candidates, tx, sequenceFlag = sequenceFlag)
      val weight = change.map(tx1.addOutput).getOrElse(tx1).weight(Protocol.PROTOCOL_VERSION)
      Transactions.weight2fee(feeRatePerKw, weight)
    }

    val changeKey = firstUnusedChangeKey.getOrElse(changeKeys.head)
    val changeScript = ewt.computePublicKeyScript(changeKey.publicKey)
    val changeTxOut = TxOut(Satoshi(0), changeScript)

    @tailrec
    def loop(current: Seq[Utxo], remaining: Seq[Utxo] = Nil): (Seq[Utxo], Option[TxOut]) = current.map(_.item.value).sum.sat match {
      case total if total - computeFee(current, None) < amount && remaining.isEmpty => throw new RuntimeException("Insufficient funds")
      case total if total - computeFee(current, None) < amount => loop(remaining.head +: current, remaining.tail)

      case total if total - computeFee(current, None) <= amount + dustLimit => (current, None)
      case total if total - computeFee(current, changeTxOut.asSome) <= amount + dustLimit && remaining.isEmpty => (current, None)
      case total if total - computeFee(current, changeTxOut.asSome) <= amount + dustLimit => loop(remaining.head +: current, remaining.tail)
      case total => (current, changeTxOut.copy(amount = total - computeFee(current, changeTxOut.asSome) - amount).asSome)
    }

    val (selected, changeOpt) = loop(Seq.empty, usable)
    val tx1 = ewt.addUtxosWithDummySig(selected, tx, sequenceFlag)
    val tx2 = changeOpt.map(tx1.addOutput).getOrElse(tx1)
    val tx3 = ewt.signTransaction(utxos, tx2)

    val fee = selected.map(_.item.value.sat).sum - tx3.txOut.map(_.amount).sum
    require(tx.txIn.isEmpty, "Cannot complete a tx that already has inputs")
    require(amount > dustLimit, "Amount to send is below dust limit")
    TxAndFee(tx3, fee)
  }

  def commitTransaction(tx: Transaction): ElectrumData = {
    // Remove all our utxos spent by this tx, call this method if the tx was broadcast successfully.
    // Since we base our utxos computation on the history from server, we need to update the history right away if we want to be able to build chained unconfirmed transactions.
    // A few seconds later electrum will notify us and the entry will be overwritten. Note that we need to take into account both inputs and outputs, because there may be change.
    val incomingScripts = tx.txIn.filter(isMine).flatMap(ewt.extractPubKeySpentFrom).map(ewt.computeScriptHashFromPublicKey)
    val outgoingScripts = tx.txOut.filter(isMine).map(_.publicKeyScript).map(computeScriptHash)
    val scripts = incomingScripts ++ outgoingScripts

    val history2 =
      scripts.foldLeft(history) {
        case (history1, scriptHash) =>
          val entry = history1.get(scriptHash) match {
            case Some(items) if items.map(_.txHash).contains(tx.txid) => items
            case Some(items) => TransactionHistoryItem(0, tx.txid) :: items
            case None => TransactionHistoryItem(0, tx.txid) :: Nil
          }

          history1.updated(scriptHash, entry)
      }

    val transactions1 = transactions.updated(tx.txid, tx)
    copy(transactions = transactions1, history = history2)
  }

  def spendAll(publicKeyScript: ByteVector, extraUtxos: List[TxOut], feeRatePerKw: FeeratePerKw, dustLimit: Satoshi, sequenceFlag: Long): TxAndFee = {
    val tx1 = ewt.addUtxosWithDummySig(utxos, Transaction(version = 2, Nil, TxOut(balance.totalBalance, publicKeyScript) :: extraUtxos, lockTime = 0), sequenceFlag)
    val fee = Transactions.weight2fee(weight = tx1.weight(Protocol.PROTOCOL_VERSION), feeratePerKw = feeRatePerKw)
    require(balance.totalBalance - fee > dustLimit, "Resulting tx amount to send is below dust limit")
    val tx2 = tx1.copy(txOut = TxOut(balance.totalBalance - fee, publicKeyScript) :: extraUtxos)
    TxAndFee(ewt.signTransaction(utxos, tx2), fee)
  }
}

case class PersistentData(accountKeysCount: Int, changeKeysCount: Int,
                          status: Map[ByteVector32, String] = Map.empty, transactions: Map[ByteVector32, Transaction] = Map.empty,
                          heights: Map[ByteVector32, Int] = Map.empty, history: Map[ByteVector32, TransactionHistoryItemList] = Map.empty,
                          proofs: Map[ByteVector32, GetMerkleResponse] = Map.empty, pendingTransactions: List[Transaction] = Nil)
