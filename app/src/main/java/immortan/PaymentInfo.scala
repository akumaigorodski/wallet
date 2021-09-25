package immortan

import fr.acinq.eclair._
import immortan.utils.ImplicitJsonFormats._
import immortan.utils.{LNUrl, PayRequest, PayRequestMeta, PaymentRequestExt}
import immortan.crypto.Tools.{Any2Some, Bytes, Fiat2Btc, SEPARATOR}
import fr.acinq.eclair.channel.{DATA_CLOSING, HasNormalCommitments}
import fr.acinq.bitcoin.{ByteVector32, Satoshi, Transaction}
import fr.acinq.eclair.wire.{FullPaymentTag, PaymentTagTlv}
import immortan.fsm.{SendMultiPart, SplitInfo}
import immortan.ChannelMaster.TxConfirmedAtOpt
import org.bouncycastle.util.encoders.Base64
import fr.acinq.bitcoin.Crypto.PublicKey
import scodec.bits.ByteVector
import java.util.Date
import scala.util.Try


object PaymentInfo {
  final val NO_ACTION = "no-action"
  final val NOT_SENDABLE_IN_FLIGHT = 0
  final val NOT_SENDABLE_SUCCESS = 1
}

object PaymentStatus {
  final val SUCCEEDED = 3
  final val ABORTED = 2
  final val PENDING = 1
  final val INIT = 0
}

case class SplitParams(prExt: PaymentRequestExt, action: Option[PaymentAction], description: PaymentDescription, cmd: SendMultiPart, chainFee: MilliSatoshi)

object SemanticOrder {
  type SemanticGroup = Seq[TransactionDetails]

  private def orderIdOrBaseId(details: TransactionDetails) = details.description.semanticOrder match { case Some(ord) => ord.id case None => details.identity }

  private def parentAndOrderId(details: TransactionDetails) = details.description.semanticOrder match { case Some(ord) => !ord.isParent -> ord.order case None => true -> Long.MaxValue }

  private def collapseChildren(items: SemanticGroup) = {
    items.tail.foreach(_.isExpandedItem = false)
    items.head.isExpandedItem = true
    items
  }

  def makeSemanticOrder(items: SemanticGroup): SemanticGroup =
    items.distinct.groupBy(orderIdOrBaseId).mapValues(_ sortBy parentAndOrderId)
      .mapValues(collapseChildren).values.toList.sortBy(_.head.seenAt)(Ordering[Long].reverse)
      .flatten
}

case class SemanticOrder(id: String, isParent: Boolean, order: Long)

sealed trait TransactionDescription {
  val semanticOrder: Option[SemanticOrder]
  val label: Option[String]
}

sealed trait TransactionDetails {
  var isExpandedItem: Boolean = true
  // We order items on UI by when they were first seen
  // We hide items depending on when they were updated
  def updatedAt: Long
  def seenAt: Long

  val date: Date = new Date(seenAt)
  val description: TransactionDescription
  val identity: String
}

case class LNUrlDescription(label: Option[String], semanticOrder: Option[SemanticOrder],
                            privKey: String, lastHash: ByteVector32, lastSecret: ByteVector32,
                            lastMsat: MilliSatoshi) extends TransactionDescription {

  lazy val fullTag: FullPaymentTag = FullPaymentTag(lastHash, lastSecret, PaymentTagTlv.LOCALLY_SENT)
}

case class LNUrlPayLink(domain: String, payString: String, payMetaString: String, updatedAt: Long, description: LNUrlDescription,
                        lastNodeIdString: String, lastCommentString: String) extends TransactionDetails {

  override val seenAt: Long = updatedAt

  override val identity: String = payString

  lazy val payLink: Option[LNUrl] = Try(payString).map(LNUrl.apply).toOption

  lazy val payMetaData: Try[PayRequestMeta] = Try(payMetaString) map to[PayRequest.TagsAndContents] map PayRequestMeta

  lazy val imageBytes: Option[Bytes] = payMetaData.map(_.imageBase64s.head).map(Base64.decode).toOption

  lazy val lastComment: Option[String] = Option(lastCommentString).filter(_.nonEmpty)
}

case class DelayedRefunds(txToParent: Map[Transaction, TxConfirmedAtOpt] = Map.empty) extends TransactionDetails {
  val totalAmount: MilliSatoshi = txToParent.keys.flatMap(_.txOut).map(_.amount).sum.toMilliSatoshi

  override val updatedAt: Long = System.currentTimeMillis * 2

  override val seenAt: Long = System.currentTimeMillis * 2

  override val identity: String = "DelayedRefunds"

  override val description: TransactionDescription = new TransactionDescription {
    val semanticOrder: Option[SemanticOrder] = None
    val label: Option[String] = None
  }
}

// Payment descriptions

sealed trait PaymentDescription extends TransactionDescription {
  val externalInfo: Option[String] // The one which comes from invoice description, LNURL-PAY metadata, etc...
  val proofTxid: Option[String] // If this is an incoming HC-routed payment with revealed preimage and stalling Host
  val split: Option[SplitInfo]
  val invoiceText: String
  val queryText: String
}

case class PlainDescription(split: Option[SplitInfo], label: Option[String], semanticOrder: Option[SemanticOrder],
                            proofTxid: Option[String], invoiceText: String) extends PaymentDescription {

  val externalInfo: Option[String] = Some(invoiceText).find(_.nonEmpty)

  val queryText: String = invoiceText + SEPARATOR + label.getOrElse(new String)
}

case class PlainMetaDescription(split: Option[SplitInfo], label: Option[String], semanticOrder: Option[SemanticOrder],
                                proofTxid: Option[String], invoiceText: String, meta: String) extends PaymentDescription {

  val externalInfo: Option[String] = List(meta, invoiceText).find(_.nonEmpty)

  val queryText: String = invoiceText + SEPARATOR + meta + SEPARATOR + label.getOrElse(new String)
}

case class PaymentInfo(prString: String, preimage: ByteVector32, status: Int, seenAt: Long, updatedAt: Long, description: PaymentDescription, actionString: String,
                       paymentHash: ByteVector32, paymentSecret: ByteVector32, received: MilliSatoshi, sent: MilliSatoshi, fee: MilliSatoshi, balanceSnapshot: MilliSatoshi,
                       fiatRatesString: String, chainFee: MilliSatoshi, incoming: Long) extends TransactionDetails {

  override val identity: String = prString

  lazy val isIncoming: Boolean = 1 == incoming

  lazy val fullTag: FullPaymentTag = FullPaymentTag(paymentHash, paymentSecret, if (isIncoming) PaymentTagTlv.FINAL_INCOMING else PaymentTagTlv.LOCALLY_SENT)

  lazy val action: Option[PaymentAction] = if (actionString == PaymentInfo.NO_ACTION) None else to[PaymentAction](actionString).asSome

  lazy val prExt: PaymentRequestExt = PaymentRequestExt.fromRaw(prString)

  lazy val fiatRateSnapshot: Fiat2Btc = to[Fiat2Btc](fiatRatesString)
}

// Payment actions

sealed trait PaymentAction {
  val domain: Option[String]
  val finalMessage: String
}

case class MessageAction(domain: Option[String], message: String) extends PaymentAction {
  val finalMessage = s"<br>${message take 144}"
}

case class UrlAction(domain: Option[String], description: String, url: String) extends PaymentAction {
  require(domain.map(_.toLowerCase).forall(url.toLowerCase.contains), "Payment action domain mismatch")
  val finalMessage = s"<br>${description take 144}<br><br><tt>$url</tt><br>"
}

case class AESAction(domain: Option[String], description: String, ciphertext: String, iv: String) extends PaymentAction {
  val ciphertextBytes: Bytes = ByteVector.fromValidBase64(ciphertext).take(1024 * 4).toArray // up to ~2kb of encrypted data
  val ivBytes: Bytes = ByteVector.fromValidBase64(iv).take(24).toArray // 16 bytes
  val finalMessage = s"<br>${description take 144}"
}

// Relayed preimages

case class RelayedPreimageInfo(paymentHashString: String, paymentSecretString: String,
                               preimageString: String, relayed: MilliSatoshi, earned: MilliSatoshi,
                               seenAt: Long, updatedAt: Long) extends TransactionDetails {

  override val identity: String = paymentHashString + paymentSecretString

  lazy val preimage: ByteVector32 = ByteVector32.fromValidHex(preimageString)

  lazy val paymentHash: ByteVector32 = ByteVector32.fromValidHex(paymentHashString)

  lazy val paymentSecret: ByteVector32 = ByteVector32.fromValidHex(paymentSecretString)

  lazy val fullTag: FullPaymentTag = FullPaymentTag(paymentHash, paymentSecret, PaymentTagTlv.TRAMPLOINE_ROUTED)

  override val description: TransactionDescription = new TransactionDescription {
    val semanticOrder: Option[SemanticOrder] = None
    val label: Option[String] = None
  }
}

// Tx descriptions

case class TxInfo(txString: String, txidString: String, pubKeyString: String, depth: Long, receivedSat: Satoshi, sentSat: Satoshi,
                  feeSat: Satoshi, seenAt: Long, updatedAt: Long, description: TxDescription, balanceSnapshot: MilliSatoshi,
                  fiatRatesString: String, incoming: Long, doubleSpent: Long) extends TransactionDetails {

  override val identity: String = txidString

  lazy val isIncoming: Boolean = 1L == incoming
  
  lazy val isDoubleSpent: Boolean = 1L == doubleSpent

  lazy val isConfirmed: Boolean = depth >= LNParams.minDepthBlocks

  lazy val fiatRateSnapshot: Fiat2Btc = to[Fiat2Btc](fiatRatesString)

  lazy val pubKey: PublicKey = PublicKey(ByteVector fromValidHex pubKeyString)

  lazy val txid: ByteVector32 = ByteVector32.fromValidHex(txidString)

  lazy val tx: Transaction = Transaction.read(txString)
}

sealed trait TxDescription extends TransactionDescription {
  def queryText(txid: ByteVector32): String
  def withNodeId: Option[PublicKey] = None
  def toAddress: Option[String] = None
}

case class PlainTxDescription(addresses: List[String], label: Option[String] = None, semanticOrder: Option[SemanticOrder] = None) extends TxDescription {
  def queryText(txid: ByteVector32): String = txid.toHex + SEPARATOR + addresses.mkString(SEPARATOR) + SEPARATOR + label.getOrElse(new String)
  override def toAddress: Option[String] = addresses.headOption
}

case class OpReturnTxDescription(preimages: List[ByteVector32], label: Option[String] = None, semanticOrder: Option[SemanticOrder] = None) extends TxDescription {
  def queryText(txid: ByteVector32): String = txid.toHex + SEPARATOR + preimages.map(_.toHex).mkString(SEPARATOR) + SEPARATOR + label.getOrElse(new String)
}

sealed trait ChanTxDescription extends TxDescription {
  override def withNodeId: Option[PublicKey] = Some(nodeId)
  def nodeId: PublicKey
}

case class ChanFundingTxDescription(nodeId: PublicKey, label: Option[String] = None, semanticOrder: Option[SemanticOrder] = None) extends ChanTxDescription {
  def queryText(txid: ByteVector32): String = txid.toHex + SEPARATOR + nodeId.toString + SEPARATOR + label.getOrElse(new String)
}

case class ChanRefundingTxDescription(nodeId: PublicKey, label: Option[String] = None, semanticOrder: Option[SemanticOrder] = None) extends ChanTxDescription {
  def queryText(txid: ByteVector32): String = txid.toHex + SEPARATOR + nodeId.toString + SEPARATOR + label.getOrElse(new String)
}

case class HtlcClaimTxDescription(nodeId: PublicKey, label: Option[String] = None, semanticOrder: Option[SemanticOrder] = None) extends ChanTxDescription {
  def queryText(txid: ByteVector32): String = txid.toHex + SEPARATOR + nodeId.toString + SEPARATOR + label.getOrElse(new String)
}

case class PenaltyTxDescription(nodeId: PublicKey, label: Option[String] = None, semanticOrder: Option[SemanticOrder] = None) extends ChanTxDescription {
  def queryText(txid: ByteVector32): String = txid.toHex + SEPARATOR + nodeId.toString + SEPARATOR + label.getOrElse(new String)
}

object TxDescription {
  def define(chans: Iterable[Channel], walletAddresses: List[String], tx: Transaction): TxDescription =
    defineOpeningRelation(chans, tx) orElse defineClosingRelation(chans, tx) getOrElse PlainTxDescription(walletAddresses)

  def defineOpeningRelation(chans: Iterable[Channel], tx: Transaction): Option[TxDescription] = chans.map(_.data).collectFirst {
    case some: HasNormalCommitments if some.commitments.commitInput.outPoint.txid == tx.txid => ChanFundingTxDescription(some.commitments.remoteInfo.nodeId)
  }

  def defineClosingRelation(chans: Iterable[Channel], tx: Transaction): Option[TxDescription] = chans.map(_.data).collectFirst {
    case closing: DATA_CLOSING if closing.balanceRefunds.exists(_.txid == tx.txid) => ChanRefundingTxDescription(closing.commitments.remoteInfo.nodeId)
    case closing: DATA_CLOSING if closing.paymentLeftoverRefunds.exists(_.txid == tx.txid) => HtlcClaimTxDescription(closing.commitments.remoteInfo.nodeId)
    case closing: DATA_CLOSING if closing.revokedCommitPublished.flatMap(_.penaltyTxs).exists(_.txid == tx.txid) => PenaltyTxDescription(closing.commitments.remoteInfo.nodeId)
    case some: HasNormalCommitments if tx.txIn.exists(_.outPoint.txid == some.commitments.commitInput.outPoint.txid) => ChanRefundingTxDescription(some.commitments.remoteInfo.nodeId)
  }
}
