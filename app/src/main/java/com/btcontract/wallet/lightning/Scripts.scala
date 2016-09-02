package com.btcontract.wallet.lightning

import Tools._
import Scripts._
import org.bitcoinj.core._
import org.bitcoinj.script.ScriptOpCodes._
import org.bitcoinj.script.{ScriptBuilder, Script}
import collection.JavaConverters.seqAsJavaListConverter
import com.btcontract.wallet.helper.Digests.ripemd160
import org.bitcoinj.script.Script.ALL_VERIFY_FLAGS
import com.btcontract.wallet.Utils.Bytes
import com.btcontract.wallet.Utils.app
import util.Try


object Scripts {
  val MIN_AMOUNT_MSAT = 546000
  type Templates = List[OutputTemplate]

  // Script for an HTLC we send to them
  def scriptPubKeyHtlcSend(ourKey: ECKey, theirKey: ECKey, absTimeout: Int,
                           relTimeout: Int, rHash: Bytes, commitRevoke: Bytes) =

    new ScriptBuilder op OP_SIZE number 32 op OP_EQUALVERIFY /* check data size */ op
      OP_HASH160 op OP_DUP data ripemd160(rHash) op OP_EQUAL /* is it an r-value? */ op
      OP_SWAP data ripemd160(commitRevoke) op OP_EQUAL /* or is it a revocation preimage? */ op
      OP_ADD /* if either r-value or revocation preimage match */ op OP_IF data theirKey.getPubKey op
      OP_ELSE number absTimeout op OP_CHECKLOCKTIMEVERIFY number relTimeout op OP_NOP3 /* OP_CSV */ op
      OP_2DROP data ourKey.getPubKey op OP_ENDIF op OP_CHECKSIG

  // Script for an HTLC we receive from them
  def scriptPubKeyHtlcReceive(ourKey: ECKey, theirKey: ECKey, absTimeout: Int,
                              relTimeout: Int, rHash: Bytes, commitRevoke: Bytes) =

    new ScriptBuilder op OP_SIZE number 32 op OP_EQUALVERIFY /* check data size */ op
      OP_HASH160 op OP_DUP data ripemd160(rHash) op OP_EQUAL /* is it an r-value? */ op
      /* if it's an r-val then I can spend this output in <relTimeout> number of blocks */
      OP_IF number relTimeout op OP_NOP3 /* OP_CSV */ op OP_2DROP data ourKey.getPubKey op
      /* else they can spend output now if they know revocation or after <absTimeout> */
      OP_ELSE data ripemd160(commitRevoke) op OP_EQUAL op OP_NOTIF number absTimeout op
      OP_CHECKLOCKTIMEVERIFY op OP_DROP op OP_ENDIF data theirKey.getPubKey op
      OP_ENDIF op OP_CHECKSIG

  def pay2wsh(s: Script) = new ScriptBuilder op OP_0 data Sha256Hash.hash(s.getProgram)
  def pay2wpkh(pubKey: ECKey) = new ScriptBuilder op OP_0 data ripemd160(pubKey.getPubKeyHash)
  def multiSig2of2(ks: ECKey*) = ScriptBuilder.createRedeemScript(2, ks.asJava)

  // OP_HASH160: The input is hashed twice: first with SHA-256 and then with RIPEMD-160
  def redeemPubKey(pubKey: Bytes) = new ScriptBuilder op OP_DUP op OP_HASH160 data ripemd160(pubKey) op OP_EQUALVERIFY op OP_CHECKSIG
  def redeemSecretOrDelay(delayedKey: Bytes, keyIfSecretKnown: Bytes, relTimeout: Int, revHash: Bytes) = new ScriptBuilder op
    OP_HASH160 data ripemd160(revHash) op OP_EQUAL op OP_IF data keyIfSecretKnown op OP_ELSE number relTimeout op
    OP_NOP3 /* OP_CSV */ op OP_DROP data delayedKey op OP_ENDIF op OP_CHECKSIG

  def makeAnchorTx(ourParams: OurChannelParams, theirCommitKey: ECKey): Anchor = ???

  def makeFinalTx(inputs: Seq[TransactionInput], ourFinalKey: ECKey, theirFinalKey: ECKey, amountUs: Long,
                  amountThem: Long, fee: Long): (Transaction, proto.close_signature) = ???

  // When they publish a revoked tx, we can spend it easily
  def makePunishTx(theirTxTemplate: TxTemplate, revPreimage: Bytes,
                   privateKey: ECKey): Transaction = ???

  def signTx(ourParams: OurChannelParams, theirParams: TheirChannelParams,
             tx: Transaction, anchorAmount: Long): proto.signature = ???

  def addTheirSigAndSignTx(ourParams: OurChannelParams, theirParams: TheirChannelParams, tx: Transaction,
                           anchorAmount: Long, theirSig: proto.signature): Transaction = ???

  def claimReceivedHtlc(theirSpentTx: Transaction, htlcTemplate: HtlcTemplate,
                        paymentPreimage: Bytes, privateKey: ECKey): Transaction = ???

  def claimSentHtlc(theirSpentTx: Transaction, htlcTemplate: HtlcTemplate,
                    privateKey: ECKey): Transaction = ???

  def claimReceivedHtlcs(theirSpentTx: Transaction, theirTxTemplate: TxTemplate,
                         commitments: Commitments): Seq[Transaction] = ???

  def claimSentHtlcs(theirSpentTx: Transaction, theirTxTemplate: TxTemplate,
                     commitments: Commitments): Seq[Transaction] = ???

  // We may need this for both our and their commitments
  def makeCommitTxTemplate(ourFinalKey: ECKey, theirFinalKey: ECKey, theirDelay: Int,
                           revHash: Bytes, spec: CommitmentSpec) = {

    // Calculate how much each side gets after applying a fee
    val goodHtlcs = spec.htlcs.filter(_.add.amount_msat >= MIN_AMOUNT_MSAT).toList
    val feeMsat = (338 + 32 * goodHtlcs.size) * spec.feeRate / 2000 * 2000
    val (amountUsMsat, amountThemMsat) = applyFees(feeMsat, spec)

    // Create templates for us, them and for in-flight HTLCs
    val htlcTemplates = for (htlc <- goodHtlcs) yield HtlcTemplate(htlc, ourFinalKey, theirFinalKey, theirDelay, revHash)
    val theirTemplate = P2WSHTemplate(Coin valueOf amountUsMsat / 1000, ourFinalKey, theirFinalKey, theirDelay, revHash)
    val ourTemplate = P2WPKHTemplate(Coin valueOf amountThemMsat / 1000, theirFinalKey)

    // TxTemplate allows us to spend our tx and spend from spent commit tx
    val theirTemplateList = if (amountThemMsat >= MIN_AMOUNT_MSAT) List(theirTemplate) else Nil
    val ourTemplateList = if (amountUsMsat >= MIN_AMOUNT_MSAT) List(ourTemplate) else Nil
    TxTemplate(ourTemplateList, theirTemplateList, htlcTemplates)
  }

  // Commit tx tries to spend an anchor output
  def brokenTxCheck(tx: Transaction, anchorOutput: TransactionOutput) = Try {
    tx.getInput(0).getScriptSig.correctlySpends(tx, 0, anchorOutput.getScriptPubKey, ALL_VERIFY_FLAGS)
  }

  def applyFees(fee: Long, spec: CommitmentSpec) =
    if (spec.amountUsMsat < fee / 2) 0L -> Math.max(0L, spec.amountThemMsat - fee + spec.amountUsMsat)
    else if (spec.amountThemMsat < fee / 2) Math.max(spec.amountUsMsat - fee + spec.amountThemMsat, 0L) -> 0L
    else (spec.amountUsMsat - fee / 2, spec.amountThemMsat - fee / 2)

  def memcmp(a: Bytes, b: Bytes): Int = (a, b) match {
    case (xNone, yNone) if xNone.isEmpty & yNone.isEmpty => 0
    case (x, y) if x.length != y.length => x.length - y.length
    case (x, y) if x.head == y.head => memcmp(x.tail, y.tail)
    case (x, y) => (x.head & 0xff) - (y.head & 0xff)
  }

  def isLessThan(o1: TransactionOutput, o2: TransactionOutput): Boolean =
    if (o1.getValue equals o2.getValue) 0 > memcmp(o1.getScriptBytes, o2.getScriptBytes)
    else o1.getValue isLessThan o2.getValue
}

// TX META STRUCTURE

trait OutputTemplate {
  val txOut: TransactionOutput
  val redeemScript: Script
  val amount: Coin
}

case class HtlcTemplate(htlc: Htlc, ourKey: ECKey, theirKey: ECKey, csvTimeout: Int, revHash: Bytes) extends OutputTemplate {
  def incoming = scriptPubKeyHtlcReceive(ourKey, theirKey, locktime2Blocks(htlc.add.expiry), csvTimeout, sha2Bytes(htlc.add.r_hash), revHash)
  def outgoing = scriptPubKeyHtlcSend(ourKey, theirKey, locktime2Blocks(htlc.add.expiry), csvTimeout, sha2Bytes(htlc.add.r_hash), revHash)
  lazy val txOut = new TransactionOutput(app.params, null, amount, pay2wsh(redeemScript).build.getProgram)
  lazy val redeemScript = if (htlc.incoming) incoming.build else outgoing.build
  lazy val amount = Coin valueOf htlc.add.amount_msat / 1000
}

case class P2WSHTemplate(amount: Coin, ourFinalKey: ECKey, theirFinalKey: ECKey, csvTimeout: Int, revHash: Bytes) extends OutputTemplate {
  lazy val redeemScript = redeemSecretOrDelay(delayedKey = ourFinalKey.getPubKey, theirFinalKey.getPubKey, csvTimeout, revHash).build
  lazy val txOut = new TransactionOutput(app.params, null, amount, pay2wsh(redeemScript).build.getProgram)
}

case class P2WPKHTemplate(amount: Coin, key: ECKey) extends OutputTemplate {
  lazy val txOut = new TransactionOutput(app.params, null, amount, pay2wpkh(key).build.getProgram)
  lazy val redeemScript = redeemPubKey(key.getPubKey).build
}

case class TxTemplate(ourOut: Templates, theirOut: Templates, htlcOuts: Templates) {
  def ordredOutputs = (ourOut ::: theirOut ::: htlcOuts).map(_.txOut) sortWith isLessThan
  def weHaveAnOutput = ourOut.nonEmpty || htlcOuts.nonEmpty

  def makeTx(prevCommitTx: Transaction): Transaction = ???
  def makeTx(anchorInput: TransactionInput): Transaction = ???
}