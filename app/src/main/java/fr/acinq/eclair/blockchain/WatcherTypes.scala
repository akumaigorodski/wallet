package fr.acinq.eclair.blockchain

import scala.util.{Success, Try}
import fr.acinq.bitcoin.{ByteVector32, Script, ScriptWitness, Transaction}
import fr.acinq.eclair.channel.BitcoinEvent
import fr.acinq.bitcoin.Crypto.PublicKey
import scodec.bits.ByteVector
import akka.actor.ActorRef


sealed trait Watch {
  def replyTo: ActorRef
  def event: BitcoinEvent
}

/**
 * Watch for confirmation of a given transaction.
 *
 * @param replyTo         actor to notify once the transaction is confirmed.
 * @param txId            txid of the transaction to watch.
 * @param publicKeyScript when using electrum, we need to specify a public key script; any of the output scripts should work.
 * @param minDepth        number of confirmations.
 * @param event           channel event related to the transaction.
 */
final case class WatchConfirmed(replyTo: ActorRef, txId: ByteVector32, publicKeyScript: ByteVector, minDepth: Long, event: BitcoinEvent) extends Watch

object WatchConfirmed {
  // if we have the entire transaction, we can get the publicKeyScript from any of the outputs
  def apply(replyTo: ActorRef, tx: Transaction, event: BitcoinEvent, minDepth: Long): WatchConfirmed =
    WatchConfirmed(replyTo, tx.txid, tx.txOut.map(_.publicKeyScript).headOption.getOrElse(ByteVector.empty), minDepth, event)

  def extractPublicKeyScript(witness: ScriptWitness): ByteVector = Try(PublicKey fromBin witness.stack.last) match {
    case Success(pubKey) => Script.write(Script pay2wpkh pubKey) // if last element of the witness is a public key, then this is a p2wpkh
    case _ => Script.write(Script pay2wsh witness.stack.last) // otherwise this is a p2wsh
  }
}

/**
 * Watch for transactions spending the given outpoint.
 *
 * NB: an event will be triggered *every time* a transaction spends the given outpoint. This can be useful when:
 *  - we see a spending transaction in the mempool, but it is then replaced (RBF)
 *  - we see a spending transaction in the mempool, but a conflicting transaction "wins" and gets confirmed in a block
 *
 * @param replyTo         actor to notify when the outpoint is spent.
 * @param txId            txid of the outpoint to watch.
 * @param outputIndex     index of the outpoint to watch.
 * @param publicKeyScript electrum requires us to specify a public key script; the script of the outpoint must be provided.
 * @param event           channel event related to the outpoint.
 * @param hints           txids of potential spending transactions; most of the time we know the txs, and it allows for optimizations.
 *                        This argument can safely be ignored by watcher implementations.
 */
final case class WatchSpent(replyTo: ActorRef, txId: ByteVector32, outputIndex: Int, publicKeyScript: ByteVector, event: BitcoinEvent, hints: Set[ByteVector32] = Set.empty) extends Watch

object WatchSpent {
  // if we have the entire transaction, we can get the publicKeyScript from the relevant output
  def apply(replyTo: ActorRef, tx: Transaction, outputIndex: Int, event: BitcoinEvent): WatchSpent =
    WatchSpent(replyTo, tx.txid, outputIndex, tx.txOut(outputIndex).publicKeyScript, event)
}

trait WatchEvent {
  def event: BitcoinEvent
}

final case class TxConfirmedAt(blockHeight: Int, tx: Transaction)

final case class WatchEventConfirmed(event: BitcoinEvent, txConfirmedAt: TxConfirmedAt, txIndex: Int) extends WatchEvent

final case class WatchEventSpent(event: BitcoinEvent, tx: Transaction) extends WatchEvent

final case class PublishAsap(tx: Transaction)

final case class GetTxWithMeta(txid: ByteVector32)

final case class GetTxWithMetaResponse(txid: ByteVector32, tx_opt: Option[Transaction], lastBlockTimestamp: Long)