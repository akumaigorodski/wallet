package fr.acinq.eclair.blockchain

import akka.actor.ActorRef
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.{ByteVector32, Script, ScriptWitness, Transaction}
import fr.acinq.eclair.channel.BitcoinEvent
import scodec.bits.ByteVector

import scala.util.{Success, Try}


sealed trait Watch {
  def replyTo: ActorRef
  def event: BitcoinEvent
}

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