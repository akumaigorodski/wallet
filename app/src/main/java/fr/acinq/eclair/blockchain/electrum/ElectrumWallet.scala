package fr.acinq.eclair.blockchain.electrum

import java.util.concurrent.ConcurrentHashMap

import akka.actor.{Actor, ActorRef, PoisonPill}
import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.bitcoin._
import fr.acinq.eclair.MilliSatoshi
import fr.acinq.eclair.blockchain.EclairWallet._
import fr.acinq.eclair.blockchain.bitcoind.rpc.Error
import fr.acinq.eclair.blockchain.electrum.Blockchain.RETARGETING_PERIOD
import fr.acinq.eclair.blockchain.electrum.ElectrumClient._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._
import fr.acinq.eclair.blockchain.electrum.db.sqlite.SqliteWalletDb.persistentDataCodec
import fr.acinq.eclair.blockchain.electrum.db.{HeaderDb, WalletDb}
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import immortan.crypto.Tools._
import immortan.sqlite.SQLiteTx
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.{immutable, mutable}
import scala.concurrent.duration._
import scala.math.min
import scala.util.{Success, Try}


class ElectrumWallet(client: ActorRef, chainSync: ActorRef, params: WalletParameters, ewt: ElectrumWalletType) extends Actor { me =>
  private def update(data: ElectrumData) = datas.update(data.ewt.xPub, data)
  private def goto(state1: State) = state = state1
  var state: State = DISCONNECTED

  def persistAndNotify(d: ElectrumData): Unit = {
    context.system.scheduler.scheduleOnce(100.millis, self, KEY_REFILL)(context.system.dispatcher)

    if (d.lastReadyMessage contains d.currentReadyMessage) update(d) else {
      val updatedData = d.copy(lastReadyMessage = d.currentReadyMessage.asSome)
      params.walletDb.persist(updatedData.toPersistent, updatedData.balance, ewt.xPub.publicKey)
      context.system.eventStream.publish(updatedData.currentReadyMessage)
      update(updatedData)
    }
  }

  context.system.eventStream.subscribe(channel = classOf[Blockchain], subscriber = self)

  client ! ElectrumClient.AddStatusListener(self)

  override def receive: Receive = {
    case anyElectrumWalletMessage: Any =>
      (datas.get(ewt.xPub), anyElectrumWalletMessage, state) match {
        case (None, rawPersistentElectrumData: ByteVector, DISCONNECTED) =>
          val persisted = Try(persistentDataCodec.decode(rawPersistentElectrumData.toBitVector).require.value).getOrElse(params.emptyPersistentData)
          val firstAccountKeys = for (idx <- math.max(persisted.accountKeysCount - 1000, 0) until persisted.accountKeysCount) yield derivePublicKey(ewt.accountMaster, idx)
          val firstChangeKeys = for (idx <- math.max(persisted.changeKeysCount - 1000, 0) until persisted.changeKeysCount) yield derivePublicKey(ewt.changeMaster, idx)

          me update ElectrumData(ewt, Blockchain(ewt.chainHash, checkpoints = Vector.empty, headersMap = Map.empty, bestchain = Vector.empty), firstAccountKeys.toVector,
            firstChangeKeys.toVector, persisted.excludedOutPoints, persisted.status.withDefaultValue(new String), persisted.transactions, persisted.overriddenPendingTxids,
            persisted.history, persisted.proofs, pendingHistoryRequests = Set.empty, pendingHeadersRequests = Set.empty, pendingTransactionRequests = Set.empty,
            pendingTransactions = persisted.pendingTransactions)

        case (Some(data), blockchain1: Blockchain, DISCONNECTED) =>
          for (scriptHash <- data.accountKeyMap.keys) client ! ElectrumClient.ScriptHashSubscription(scriptHash, self)
          for (scriptHash <- data.changeKeyMap.keys) client ! ElectrumClient.ScriptHashSubscription(scriptHash, self)
          val data1 = data.copy(blockchain = blockchain1)
          persistAndNotify(data1)
          goto(RUNNING)

        case (Some(data), blockchain1: Blockchain, RUNNING) =>
          val data1 = data.copy(blockchain = blockchain1)
          data1.pendingMerkleResponses.foreach(self.!)
          persistAndNotify(data1)

        case (Some(data), ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, status), RUNNING) if data.status.get(scriptHash).contains(status) =>
          val missing = data.history.getOrElse(scriptHash, Nil).map(item => item.txHash -> item.height).toMap -- data.transactions.keySet -- data.pendingTransactionRequests

          missing.foreach { case (txId, height) =>
            client ! GetTransaction(txid = txId)
            client ! GetMerkle(txId, height)
          }

          if (missing.nonEmpty) {
            // An optimization to not recalculate internal data values on each scriptHashResponse event
            val data1 = data.copy(pendingHistoryRequests = data.pendingTransactionRequests ++ missing.keySet)
            persistAndNotify(data1)
          }

        case (Some(data), ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, status), RUNNING) if status.isEmpty =>
          // Generally we don't need to do anything here because most likely this is a fresh address where scriptHash is already empty
          // But if we did have something there while now there is nothing then an incoming transaction has likely been cancelled
          if (data.status.get(scriptHash).nonEmpty) client ! ElectrumClient.GetScriptHashHistory(scriptHash)

        case (Some(data), ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, status), RUNNING) =>
          val data1 = data.copy(status = data.status.updated(scriptHash, status), pendingHistoryRequests = data.pendingHistoryRequests + scriptHash)
          client ! ElectrumClient.GetScriptHashHistory(scriptHash)
          persistAndNotify(data1)

        case (Some(data), ElectrumClient.GetScriptHashHistoryResponse(scriptHash, items), RUNNING) =>
          val pendingHeadersRequests1 = collection.mutable.HashSet.empty[GetHeaders]
          pendingHeadersRequests1 ++= data.pendingHeadersRequests

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
            if (data.proofs.contains(txid) || height < 1) return
            downloadHeadersIfMissing(height)
            client ! GetMerkle(txid, height)
          }

          val pendingTransactionRequests1 = items.foldLeft(data.pendingTransactionRequests) {
            case (hashes, item) if !data.transactions.contains(item.txHash) && !data.pendingTransactionRequests.contains(item.txHash) =>
              client ! GetTransaction(item.txHash)
              process(item.txHash, item.height)
              hashes + item.txHash

            case (hashes, item) =>
              process(item.txHash, item.height)
              hashes
          }

          val shadowItems = for {
            existingItems <- data.history.get(scriptHash).toList
            item <- existingItems if !items.exists(_.txHash == item.txHash)
          } yield item.txHash

          val pendingTransactions1 = data.pendingTransactions.filterNot { pendingTx =>
            // We have a transaction that Electrum does not know about, remove it
            shadowItems.contains(pendingTx.txid)
          }

          val data1 = data.copy(history = data.history.updated(scriptHash, items),
            pendingHistoryRequests = data.pendingHistoryRequests - scriptHash,
            pendingTransactionRequests = pendingTransactionRequests1,
            pendingHeadersRequests = pendingHeadersRequests1.toSet,
            transactions = data.transactions -- shadowItems,
            pendingTransactions = pendingTransactions1)
          persistAndNotify(data1)

        case (Some(data), GetTransactionResponse(tx), RUNNING) =>
          val clearedExcludedOutPoints: List[OutPoint] = data.excludedOutPoints diff tx.txIn.map(_.outPoint)
          // Even though we have excluded some utxos in this wallet user may still spend them from other wallet, so clear excluded outpoints here
          val data1 = data.copy(pendingTransactionRequests = data.pendingTransactionRequests - tx.txid, excludedOutPoints = clearedExcludedOutPoints)

          val data2 = data.computeTransactionDelta(tx) map { case TransactionDelta(_, fee, received, sent) =>
            val addresses = tx.txOut.filter(data.isMine).map(_.publicKeyScript).flatMap(data.publicScriptMap.get).map(ewt.textAddress).toList
            context.system.eventStream publish TransactionReceived(tx, data.depth(tx.txid), data.timestamp(tx.txid, params.headerDb), received, sent, addresses, List(ewt.xPub), fee)
            // Transactions arrive asynchronously, we may have valid txs wihtout parents, if that happens we wait until parents arrive and then retry delta check again
            for (stillPendingTx <- data.pendingTransactions) self ! GetTransactionResponse(stillPendingTx)
            data1.copy(transactions = data.transactions.updated(tx.txid, tx), pendingTransactions = Nil)
          } getOrElse data1.copy(pendingTransactions = data.pendingTransactions :+ tx)
          persistAndNotify(data2)

        case (Some(data), response: GetMerkleResponse, RUNNING) =>
          val request = GetHeaders(response.blockHeight / RETARGETING_PERIOD * RETARGETING_PERIOD, RETARGETING_PERIOD)

          data.blockchain.getHeader(response.blockHeight) orElse params.headerDb.getHeader(response.blockHeight) match {
            case Some(existingMerkleHeader) if existingMerkleHeader.hashMerkleRoot == response.root && data.isTxKnown(response.txid) =>
              val data1 = data.copy(proofs = data.proofs.updated(response.txid, response), pendingMerkleResponses = data.pendingMerkleResponses - response)
              persistAndNotify(data1.withOverridingTxids)

            case Some(existingHeader) if existingHeader.hashMerkleRoot == response.root =>
              // Do nothing but guard from this being matched further down the road

            case None if data.pendingHeadersRequests.contains(request) =>
              val data1 = data.copy(pendingMerkleResponses = data.pendingMerkleResponses + response)
              persistAndNotify(data1)

            case None =>
              chainSync ! request
              val data1 = data.copy(pendingHeadersRequests = data.pendingHeadersRequests + request)
              val data2 = data1.copy(pendingMerkleResponses = data1.pendingMerkleResponses + response)
              persistAndNotify(data2)

            case _ =>
              // Something is wrong with this client, better disconnect from it
              val data1 = data.copy(transactions = data.transactions - response.txid)
              persistAndNotify(data1)
              sender ! PoisonPill
          }

        case (_, bc: ElectrumClient.BroadcastTransaction, RUNNING) =>
          // We forward here becase it's up to client to reply to sender
          client forward bc

        case (Some(data), tx: Transaction, _) =>
          val doubleSpendTrials: Option[Boolean] = for {
            spendingTxid <- data.overriddenPendingTxids.get(tx.txid)
            spendingBlockHeight <- data.proofs.get(spendingTxid).map(_.blockHeight)
          } yield data.computeDepth(spendingBlockHeight) > 0

          val depth = data.depth(tx.txid)
          val stamp = data.timestamp(tx.txid, params.headerDb)
          // Has been replaced by one of our txs or somehow we know nothing about it
          val isDoubleSpent = doubleSpendTrials.contains(true) || !data.isTxKnown(tx.txid)
          sender ! IsDoubleSpentResponse(tx, depth, stamp, isDoubleSpent)

        case (Some(data), GetCurrentReceiveAddresses, _) =>
          val changeKey = data.firstUnusedChangeKeys.headOption.getOrElse(data.changeKeys.head)
          val sortredAccountKeys = data.firstUnusedAccountKeys.toList.sortBy(_.path.lastChildNumber)
          sender ! GetCurrentReceiveAddressesResponse(sortredAccountKeys, changeKey, ewt)

        case (Some(dirtyData), ElectrumClient.ElectrumDisconnected, _) =>
          update(dirtyData.reset)
          goto(DISCONNECTED)

        case (Some(data), ProvideExcludedOutPoints(excluded), _) =>
          val data1 = data.copy(excludedOutPoints = excluded)
          persistAndNotify(data1)

        case (Some(data), GetData, _) => sender ! GetDataResponse(data)

        case (Some(data), CompleteTransaction(pubKeyScriptToAmount, feeRatePerKw, seqFlag), _) =>
          val txOuts = for (script ~ amount <- pubKeyScriptToAmount) yield TxOut(amount, script)
          val tx = Transaction(version = 2, txIn = Nil, txOut = txOuts.toList, lockTime = 0)

          val changeScript = data.ewt.computePublicKeyScript(data.firstUnusedChangeKeys.headOption.getOrElse(data.changeKeys.head).publicKey)
          val resultTry = ElectrumWallet.completeTransaction(tx, feeRatePerKw, params.dustLimit, seqFlag, changeScript, data.utxos)
          val resultTry1 = for (res <- resultTry) yield res.copy(pubKeyScriptToAmount = pubKeyScriptToAmount)
          sender ! resultTry1

        case (Some(data), SendAll(publicKeyScript, pubKeyScriptToAmount, feeRatePerKw, seqFlag, fromOutpoints, extraOutUtxos), _) =>
          val inUtxos = if (fromOutpoints.nonEmpty) data.utxos.filter(utxo => fromOutpoints contains utxo.item.outPoint) else data.utxos
          sender ! ElectrumWallet.spendAll(publicKeyScript, pubKeyScriptToAmount, inUtxos, extraOutUtxos, feeRatePerKw, params.dustLimit, seqFlag)

        case (Some(data), bump: RBFBump, _) if bump.tx.txIn.forall(_.sequence <= OPT_IN_FULL_RBF) => sender ! ElectrumWallet.rbfBump(bump, params.dustLimit, data)
        case (Some(data), reroute: RBFReroute, _) if reroute.tx.txIn.forall(_.sequence <= OPT_IN_FULL_RBF) => sender ! ElectrumWallet.rbfReroute(reroute, params.dustLimit, data)

        case (_, _: RBFBump, _) => sender ! RBFResponse(RBF_DISABLED.asLeft)
        case (_, _: RBFReroute, _) => sender ! RBFResponse(RBF_DISABLED.asLeft)

        case (_, ElectrumClient.BroadcastTransaction(tx), _) =>
          val notConnected = Error(code = -1, "wallet is not connected").asSome
          sender ! ElectrumClient.BroadcastTransactionResponse(tx, notConnected)

        case (Some(data), KEY_REFILL, _) if data.firstUnusedChangeKeys.size < MAX_RECEIVE_ADDRESSES =>
          val newKey = derivePublicKey(ewt.changeMaster, data.changeKeys.last.path.lastChildNumber + 1)
          val newKeyScriptHash = computeScriptHash(ewt writePublicKeyScriptHash newKey.publicKey)
          client ! ElectrumClient.ScriptHashSubscription(newKeyScriptHash, self)
          val data1 = data.copy(changeKeys = data.changeKeys :+ newKey)
          persistAndNotify(data1)

        case (Some(data), KEY_REFILL, _) if data.firstUnusedAccountKeys.size < MAX_RECEIVE_ADDRESSES =>
          val newKey = derivePublicKey(ewt.accountMaster, data.accountKeys.last.path.lastChildNumber + 1)
          val newKeyScriptHash = computeScriptHash(ewt writePublicKeyScriptHash newKey.publicKey)
          client ! ElectrumClient.ScriptHashSubscription(newKeyScriptHash, self)
          val data1 = data.copy(accountKeys = data.accountKeys :+ newKey)
          persistAndNotify(data1)

        case _ =>
          // Do nothing
      }
  }
}

object ElectrumWallet {
  type TxOutOption = Option[TxOut]
  type TxHistoryItemList = List[TransactionHistoryItem]

  val datas: mutable.Map[ExtendedPublicKey, ElectrumData] =
    new ConcurrentHashMap[ExtendedPublicKey, ElectrumData].asScala

  final val KEY_REFILL = "key-refill"
  final val GENERATION_FAIL = 0
  final val PARENTS_MISSING = 1
  final val FOREIGN_INPUTS = 2
  final val RBF_DISABLED = 3

  sealed trait State
  case object DISCONNECTED extends State
  case object WAITING_FOR_TIP extends State
  case object SYNCING extends State
  case object RUNNING extends State

  sealed trait Request
  sealed trait Response

  sealed trait GenerateTxResponse extends Response {
    val pubKeyScriptToAmount: Map[ByteVector, Satoshi]
    // This is guaranteed to exclude our own change output
    lazy val transferred: Satoshi = pubKeyScriptToAmount.values.sum
    val tx: Transaction
    val fee: Satoshi
  }

  case object GetData extends Request
  case class GetDataResponse(data: ElectrumData) extends Response

  case class ProvideExcludedOutPoints(excludedOutPoints: List[OutPoint] = Nil) extends Request

  case object GetCurrentReceiveAddresses extends Request
  case class GetCurrentReceiveAddressesResponse(keys: List[ExtendedPublicKey], changeKey: ExtendedPublicKey, ewt: ElectrumWalletType) extends Response {
    def firstAccountAddress: String = ewt.textAddress(keys.head)
    def changeAddress: String = ewt.textAddress(changeKey)
  }

  case class CompleteTransaction(pubKeyScriptToAmount: Map[ByteVector, Satoshi], feeRatePerKw: FeeratePerKw, sequenceFlag: Long) extends Request
  case class CompleteTransactionResponse(pubKeyScriptToAmount: Map[ByteVector, Satoshi], tx: Transaction, fee: Satoshi) extends GenerateTxResponse

  case class SendAll(publicKeyScript: ByteVector, pubKeyScriptToAmount: Map[ByteVector, Satoshi], feeRatePerKw: FeeratePerKw, sequenceFlag: Long, fromOutpoints: Set[OutPoint], extraUtxos: List[TxOut] = Nil) extends Request
  case class SendAllResponse(pubKeyScriptToAmount: Map[ByteVector, Satoshi], tx: Transaction, fee: Satoshi) extends GenerateTxResponse

  case class RBFBump(tx: Transaction, feeRatePerKw: FeeratePerKw, sequenceFlag: Long) extends Request
  case class RBFReroute(tx: Transaction, feeRatePerKw: FeeratePerKw, publicKeyScript: ByteVector, sequenceFlag: Long) extends Request
  case class RBFResponse(result: Either[Int, GenerateTxResponse] = GENERATION_FAIL.asLeft) extends Response

  case class ChainFor(target: ActorRef) extends Request

  case class IsDoubleSpentResponse(tx: Transaction, depth: Long, stamp: Long, isDoubleSpent: Boolean) extends Response

  sealed trait WalletEvent
  case class WalletReady(balance: Satoshi, height: Long, heightsCode: Int, xPub: ExtendedPublicKey, unExcludedUtxos: Seq[Utxo], excludedOutPoints: List[OutPoint] = Nil) extends WalletEvent
  case class TransactionReceived(tx: Transaction, depth: Long, stamp: Long, received: Satoshi, sent: Satoshi, addresses: StringList, xPubs: List[ExtendedPublicKey], fee: Satoshi) extends WalletEvent {
    def merge(that: TransactionReceived) = TransactionReceived(tx, min(depth, that.depth), min(stamp, that.stamp), received + that.received, sent + that.sent, addresses ++ that.addresses, xPubs ++ that.xPubs, fee + that.fee)
  }

  def weight2feeMsat(feeratePerKw: FeeratePerKw, weight: Int): MilliSatoshi = MilliSatoshi(feeratePerKw.toLong * weight)
  def weight2fee(feeratePerKw: FeeratePerKw, weight: Int): Satoshi = weight2feeMsat(feeratePerKw, weight).truncateToSatoshi
  def completeTransaction(tx: Transaction, feeRatePerKw: FeeratePerKw, dustLimit: Satoshi, sequenceFlag: Long, changeScript: Seq[ScriptElt],
                          usableInUtxos: Seq[Utxo], mustUseUtxos: Seq[Utxo] = Nil): Try[CompleteTransactionResponse] = Try {

    def computeFee(candidates: Seq[Utxo], change: TxOutOption) = {
      val tx1 = ElectrumWalletType.dummySignTransaction(candidates, tx, sequenceFlag)
      val weight = change.map(tx1.addOutput).getOrElse(tx1).weight(Protocol.PROTOCOL_VERSION)
      weight2fee(feeRatePerKw, weight)
    }

    val amountToSend = tx.txOut.map(_.amount).sum
    val changeTxOut = TxOut(Satoshi(0L), changeScript)

    require(tx.txIn.isEmpty, "Cannot complete a tx that already has inputs")
    require(amountToSend > dustLimit, "Amount to send is below dust limit")

    @tailrec
    def loop(current: Seq[Utxo], remaining: Seq[Utxo] = Nil): (Seq[Utxo], TxOutOption) = current.map(_.item.value).sum.sat match {
      case total if total - computeFee(current, None) < amountToSend && remaining.isEmpty => throw new RuntimeException("Insufficient funds")
      case total if total - computeFee(current, None) < amountToSend => loop(remaining.head +: current, remaining.tail)

      case total if total - computeFee(current, None) <= amountToSend + dustLimit => (current, None)
      case total if total - computeFee(current, changeTxOut.asSome) <= amountToSend + dustLimit && remaining.isEmpty => (current, None)
      case total if total - computeFee(current, changeTxOut.asSome) <= amountToSend + dustLimit => loop(remaining.head +: current, remaining.tail)
      case total => (current, changeTxOut.copy(amount = total - computeFee(current, changeTxOut.asSome) - amountToSend).asSome)
    }

    val usable = usableInUtxos.sortBy(_.item.value)
    val (selected, changeOpt) = loop(current = mustUseUtxos, usable)
    val txWithInputs = ElectrumWalletType.dummySignTransaction(selected, tx, sequenceFlag)
    val txWithChange = changeOpt.map(txWithInputs.addOutput).getOrElse(txWithInputs)

    val tx3 = ElectrumWalletType.signTransaction(usableInUtxos ++ mustUseUtxos, txWithChange)
    val computedFee = selected.map(_.item.value).sum.sat - tx3.txOut.map(_.amount).sum
    CompleteTransactionResponse(Map.empty, tx3, computedFee)
  }

  def spendAll(restPubKeyScript: ByteVector, strictPubKeyScriptsToAmount: Map[ByteVector, Satoshi],
               usableInUtxos: Seq[Utxo], extraOutUtxos: List[TxOut], feeRatePerKw: FeeratePerKw,
               dustLimit: Satoshi, sequenceFlag: Long): Try[SendAllResponse] = Try {

    val strictTxOuts = for (Tuple2(pubKeyScript, amount) <- strictPubKeyScriptsToAmount) yield TxOut(amount, pubKeyScript)
    val restTxOut = TxOut(amount = usableInUtxos.map(_.item.value.sat).sum - strictTxOuts.map(_.amount).sum - extraOutUtxos.map(_.amount).sum, restPubKeyScript)
    val tx1 = ElectrumWalletType.dummySignTransaction(usableInUtxos, Transaction(version = 2, Nil, restTxOut :: strictTxOuts.toList ::: extraOutUtxos, lockTime = 0), sequenceFlag)

    val fee = weight2fee(weight = tx1.weight(Protocol.PROTOCOL_VERSION), feeratePerKw = feeRatePerKw)
    require(restTxOut.amount - fee > dustLimit, "Resulting tx amount to send is below dust limit")

    val restTxOut1 = TxOut(restTxOut.amount - fee, restPubKeyScript)
    val tx2 = tx1.copy(txOut = restTxOut1 :: strictTxOuts.toList ::: extraOutUtxos)
    val allPubKeyScriptsToAmount = strictPubKeyScriptsToAmount.updated(restPubKeyScript, restTxOut1.amount)
    SendAllResponse(allPubKeyScriptsToAmount, ElectrumWalletType.signTransaction(usableInUtxos, tx2), fee)
  }

  def rbfBump(bump: RBFBump, dustLimit: Satoshi, data: ElectrumData): RBFResponse = {
    val tx1 = bump.tx.copy(txOut = bump.tx.txOut.filterNot(data.isMine), txIn = Nil)

    data.computeTransactionDelta(bump.tx) map {
      // We sent all our funds to a single address which does not belong to us, can reroute it
      case delta if delta.fee > 0L.sat && bump.tx.txOut.size == 1 && tx1.txOut.nonEmpty && data.utxos.isEmpty =>
        rbfReroute(tx1.txOut.head.publicKeyScript, delta.spentUtxos, bump.feeRatePerKw, dustLimit, bump.sequenceFlag)

      case delta if delta.fee > 0L.sat =>
        val leftUtxos = data.utxos.filterNot(_.item.txHash == bump.tx.txid)
        val changeScript = data.ewt.computePublicKeyScript(data.firstUnusedChangeKeys.headOption.getOrElse(data.changeKeys.head).publicKey)
        completeTransaction(tx1, bump.feeRatePerKw, dustLimit, bump.sequenceFlag, changeScript, leftUtxos, delta.spentUtxos) match {
          case Success(response) => RBFResponse(response.asRight)
          case _ => RBFResponse(GENERATION_FAIL.asLeft)
        }

      case _ => RBFResponse(FOREIGN_INPUTS.asLeft)
    } getOrElse RBFResponse(PARENTS_MISSING.asLeft)
  }

  // We use this one for cancelling (rerouting onto our own address)
  def rbfReroute(reroute: RBFReroute, dustLimit: Satoshi, data: ElectrumData): RBFResponse = data.computeTransactionDelta(reroute.tx) map { delta =>
    rbfReroute(reroute.publicKeyScript, delta.spentUtxos, reroute.feeRatePerKw, dustLimit, reroute.sequenceFlag)
  } getOrElse RBFResponse(PARENTS_MISSING.asLeft)

  // This one gets a set on used utxos and makes a new tx such that higher fee is deduced from that set of utxos, so recepient will get less money
  private def rbfReroute(publicKeyScript: ByteVector, spentUtxos: Seq[Utxo], feeRatePerKw: FeeratePerKw, dustLimit: Satoshi, sequenceFlag: Long): RBFResponse = {
    spendAll(publicKeyScript, strictPubKeyScriptsToAmount = Map.empty, usableInUtxos = spentUtxos, extraOutUtxos = Nil, feeRatePerKw, dustLimit, sequenceFlag) match {
      case Success(response) => RBFResponse(response.asRight)
      case _ => RBFResponse(GENERATION_FAIL.asLeft)
    }
  }
}

case class AccountAndXPrivKey(xPriv: ExtendedPrivateKey, master: ExtendedPrivateKey)
case class Utxo(key: ExtendedPublicKey, item: ElectrumClient.UnspentItem, ewt: ElectrumWalletType)
case class TransactionDelta(spentUtxos: Seq[Utxo], fee: Satoshi, received: Satoshi, sent: Satoshi) {
  def merge(that: TransactionDelta) = TransactionDelta(spentUtxos ++ that.spentUtxos, fee + that.fee, received + that.received, sent + that.sent)
}

case class WalletParameters(headerDb: HeaderDb, walletDb: WalletDb, txDb: SQLiteTx, dustLimit: Satoshi) {
  lazy val emptyPersistentData: PersistentData = PersistentData(accountKeysCount = MAX_RECEIVE_ADDRESSES, changeKeysCount = MAX_RECEIVE_ADDRESSES)
  lazy val emptyPersistentDataBytes: ByteVector = persistentDataCodec.encode(emptyPersistentData).require.toByteVector
}

case class ElectrumData(ewt: ElectrumWalletType, blockchain: Blockchain,
                        accountKeys: Vector[ExtendedPublicKey], changeKeys: Vector[ExtendedPublicKey], excludedOutPoints: List[OutPoint],
                        status: Map[ByteVector32, String], transactions: Map[ByteVector32, Transaction], overriddenPendingTxids: Map[ByteVector32, ByteVector32],
                        history: Map[ByteVector32, TxHistoryItemList], proofs: Map[ByteVector32, GetMerkleResponse], pendingHistoryRequests: Set[ByteVector32] = Set.empty,
                        pendingTransactionRequests: Set[ByteVector32] = Set.empty, pendingHeadersRequests: Set[GetHeaders] = Set.empty, pendingTransactions: List[Transaction] = Nil,
                        pendingMerkleResponses: Set[GetMerkleResponse] = Set.empty, lastReadyMessage: Option[WalletReady] = None) { me =>

  lazy val publicScriptAccountMap: Map[ByteVector, ExtendedPublicKey] = accountKeys.map(key => ewt.writePublicKeyScriptHash(key.publicKey) -> key).toMap
  lazy val publicScriptChangeMap: Map[ByteVector, ExtendedPublicKey] = changeKeys.map(key => ewt.writePublicKeyScriptHash(key.publicKey) -> key).toMap
  lazy val publicScriptMap: Map[ByteVector, ExtendedPublicKey] = publicScriptAccountMap ++ publicScriptChangeMap

  lazy val accountKeyMap: Map[ByteVector32, ExtendedPublicKey] = for (Tuple2(serialized, key) <- publicScriptAccountMap) yield (computeScriptHash(serialized), key)
  lazy val changeKeyMap: Map[ByteVector32, ExtendedPublicKey] = for (Tuple2(serialized, key) <- publicScriptChangeMap) yield (computeScriptHash(serialized), key)

  lazy val currentReadyMessage: WalletReady = WalletReady(balance, blockchain.tip.height, proofs.hashCode + transactions.hashCode, ewt.xPub, unExcludedUtxos, excludedOutPoints)
  lazy val firstUnusedAccountKeys: immutable.Iterable[ExtendedPublicKey] = accountKeyMap.collect { case (scriptHash, privKey) if status(scriptHash) == new String => privKey }

  lazy val firstUnusedChangeKeys: immutable.Iterable[ExtendedPublicKey] = {
    val usedChangeNumbers = transactions.values.flatMap(_.txOut).map(_.publicKeyScript).flatMap(publicScriptChangeMap.get).map(_.path.lastChildNumber).toSet
    changeKeys.collect { case unusedChangeKey if !usedChangeNumbers.contains(unusedChangeKey.path.lastChildNumber) => unusedChangeKey }
  }

  lazy val unExcludedUtxos: Seq[Utxo] = {
    history.toSeq.flatMap { case (scriptHash, historyItems) =>
      accountKeyMap.get(scriptHash) orElse changeKeyMap.get(scriptHash) map { key =>
        // We definitely have a private key generated for corresponding scriptHash here

        val unspents = for {
          item <- historyItems
          tx <- transactions.get(item.txHash).toList
          if !overriddenPendingTxids.contains(tx.txid)
          (txOut, index) <- tx.txOut.zipWithIndex if computeScriptHash(txOut.publicKeyScript) == scriptHash
          unspent = ElectrumClient.UnspentItem(item.txHash, index, txOut.amount.toLong, item.height)
        } yield Utxo(key, unspent, ewt)

        // Find all out points which spend from this script hash and make sure unspents do not contain them
        val outPoints = historyItems.map(_.txHash).flatMap(transactions.get).flatMap(_.txIn).map(_.outPoint).toSet
        unspents.filterNot(utxo => outPoints contains utxo.item.outPoint)
      } getOrElse Nil
    }
  }

  lazy val utxos: Seq[Utxo] = unExcludedUtxos.filterNot(utxo => excludedOutPoints contains utxo.item.outPoint)

  lazy val balance: Satoshi = utxos.foldLeft(0L.sat)(_ + _.item.value.sat)

  // Remove status for each script hash for which we have pending requests, this will make us query script hash history for these script hashes again when we reconnect
  def reset: ElectrumData = copy(status = status -- pendingHistoryRequests, pendingHistoryRequests = Set.empty, pendingTransactionRequests = Set.empty, pendingHeadersRequests = Set.empty, lastReadyMessage = None)

  def toPersistent: PersistentData = PersistentData(accountKeys.length, changeKeys.length, status, transactions, overriddenPendingTxids, history, proofs, pendingTransactions, excludedOutPoints)

  def isTxKnown(txid: ByteVector32): Boolean = transactions.contains(txid) || pendingTransactionRequests.contains(txid) || pendingTransactions.exists(_.txid == txid)

  def isMine(txIn: TxIn): Boolean = ewt.extractPubKeySpentFrom(txIn).map(ewt.writePublicKeyScriptHash).exists(publicScriptMap.contains)

  def isMine(txOut: TxOut): Boolean = publicScriptMap.contains(txOut.publicKeyScript)

  def timestamp(txid: ByteVector32, headerDb: HeaderDb): Long = {
    val blockHeight = proofs.get(txid).map(_.blockHeight).getOrElse(default = 0)
    val stampOpt = blockchain.getHeader(blockHeight) orElse headerDb.getHeader(blockHeight)
    stampOpt.map(_.time * 1000L).getOrElse(System.currentTimeMillis)
  }

  def depth(txid: ByteVector32): Int = proofs.get(txid).map(_.blockHeight).map(computeDepth).getOrElse(0)
  def computeDepth(txHeight: Int): Int = if (txHeight <= 0L) 0 else blockchain.height - txHeight + 1

  def computeTransactionDelta(tx: Transaction): Option[TransactionDelta] = {
    // Computes the effect of this transaction on the wallet
    val ourInputs = tx.txIn.filter(isMine)

    for (txIn <- ourInputs) {
      // Can only be computed if all our inputs have confirmed parents
      val hasParent = transactions.contains(txIn.outPoint.txid)
      if (!hasParent) return None
    }

    val spentUtxos = ourInputs.map { txIn =>
      // This may be needed for RBF and it's a good place to create these UTXOs
      // we create simulated as-if yet unused UTXOs to be reused in RBF transaction
      val TxOut(amount, publicKeyScript) = transactions(txIn.outPoint.txid).txOut(txIn.outPoint.index.toInt)
      val item = UnspentItem(txIn.outPoint.txid, txIn.outPoint.index.toInt, amount.toLong, height = 0)
      Utxo(publicScriptMap(publicKeyScript), item, ewt)
    }

    val totalReceived = tx.txOut.map(_.amount).sum
    val mineSent = spentUtxos.map(_.item.value.sat).sum
    val fee = if (ourInputs.size != tx.txIn.size) 0L.sat else mineSent - totalReceived
    TransactionDelta(spentUtxos, fee, tx.txOut.filter(isMine).map(_.amount).sum, mineSent).asSome
  }

  def withOverridingTxids: ElectrumData = {
    def changeKeyDepth(tx: Transaction) = if (proofs contains tx.txid) Long.MaxValue else tx.txOut.map(_.publicKeyScript).flatMap(publicScriptChangeMap.get).map(_.path.lastChildNumber).headOption.getOrElse(Long.MinValue)
    def computeOverride(txs: Iterable[Transaction] = Nil) = Stream.continually(txs maxBy changeKeyDepth) zip txs collect { case (latterTx, formerTx) if latterTx != formerTx => formerTx.txid -> latterTx.txid }
    val overrides = transactions.values.map(Stream continually _).flatMap(txStream => txStream.head.txIn zip txStream).groupBy(_._1.outPoint).values.map(_.secondItems).flatMap(computeOverride)
    copy(overriddenPendingTxids = overrides.toMap)
  }
}

case class PersistentData(accountKeysCount: Int, changeKeysCount: Int, status: Map[ByteVector32, String] = Map.empty,
                          transactions: Map[ByteVector32, Transaction] = Map.empty, overriddenPendingTxids: Map[ByteVector32, ByteVector32] = Map.empty,
                          history: Map[ByteVector32, TxHistoryItemList] = Map.empty, proofs: Map[ByteVector32, GetMerkleResponse] = Map.empty,
                          pendingTransactions: List[Transaction] = Nil, excludedOutPoints: List[OutPoint] = Nil)
