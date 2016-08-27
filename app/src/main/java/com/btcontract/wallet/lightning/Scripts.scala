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

  def redeemPubKey(pubKey: Bytes) = new ScriptBuilder op OP_DUP op OP_HASH160 data ripemd160(pubKey) op OP_EQUALVERIFY op OP_CHECKSIG
  def redeemSecretOrDelay(delayedKey: Bytes, keyIfSecretKnown: Bytes, relTimeout: Int, hashOfSecret: Bytes) = new ScriptBuilder op
    OP_HASH160 data ripemd160(hashOfSecret) op OP_EQUAL op OP_IF data keyIfSecretKnown op OP_ELSE number relTimeout op
    OP_NOP3 /* OP_CSV */ op OP_DROP data delayedKey op OP_ENDIF op OP_CHECKSIG

  def makeAnchorTx: (Transaction, Int) = ???

  def makeCommitTx: Transaction = ???

  def makeBreachTx: Transaction = ???

  def makeFinalTx: (Transaction, proto.close_signature) = ???

  def signTx: proto.signature = ???

  def addTheirSigAndSignTx: Transaction = ???

  // Commit tx tries to spend an anchor output
  def brokenTxCheck(tx: Transaction, anchorOutput: TransactionOutput) = Try {
    tx.getInput(0).getScriptSig.correctlySpends(tx, 0, anchorOutput.getScriptPubKey, ALL_VERIFY_FLAGS)
  }

  def computeFee2(feeRate: Long, htlcNum: Int) =
    (338 + 32 * htlcNum) * feeRate / 2000 * 2

  def applyFees(us: Long, them: Long, fee: Long) =
    if (us < fee / 2) 0L -> Math.max(0L, them - fee + us)
    else if (them < fee / 2) Math.max(us - fee + them, 0L) -> 0L
    else (us - fee / 2, them - fee / 2)

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
  def txOut: TransactionOutput
  def redeemScript: Script
  def amount: Long
}

case class HtlcTemplate(htlc: Htlc, ourKey: ECKey, theirKey: ECKey, delay: Int, revHash: Bytes) extends OutputTemplate {
  def incoming = scriptPubKeyHtlcReceive(ourKey, theirKey, locktime2Blocks(htlc.add.expiry), delay, sha2Bytes(htlc.add.r_hash), revHash)
  def outgoing = scriptPubKeyHtlcSend(ourKey, theirKey, locktime2Blocks(htlc.add.expiry), delay, sha2Bytes(htlc.add.r_hash), revHash)
  def txOut = new TransactionOutput(app.params, null, Coin valueOf amount, pay2wsh(redeemScript).build.getProgram)
  def redeemScript = if (htlc.incoming) incoming.build else outgoing.build
  def amount = htlc.add.amount_msat / 1000
}

case class P2WSHTemplate(amount: Long, ourFinalKey: ECKey, theirFinalKey: ECKey, theirDelay: Int, revHash: Bytes) extends OutputTemplate {
  def redeemScript = redeemSecretOrDelay(delayedKey = ourFinalKey.getPubKey, theirFinalKey.getPubKey, relTimeout = theirDelay, revHash).build
  def txOut = new TransactionOutput(app.params, null, Coin valueOf amount, pay2wsh(redeemScript).build.getProgram)
}

case class P2WPKHTemplate(amount: Long, key: ECKey) extends OutputTemplate {
  def txOut = new TransactionOutput(app.params, null, Coin valueOf amount, pay2wpkh(key).build.getProgram)
  def redeemScript = redeemPubKey(key.getPubKey).build
}