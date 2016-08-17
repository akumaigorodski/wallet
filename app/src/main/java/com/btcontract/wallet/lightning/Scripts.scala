package com.btcontract.wallet.lightning

import org.bitcoinj.core._
import org.bitcoinj.script.ScriptOpCodes._
import org.bitcoinj.script.{ScriptBuilder, Script}
import collection.JavaConverters.seqAsJavaListConverter
import com.btcontract.wallet.helper.Digests.ripemd160
import org.bitcoinj.script.Script.ALL_VERIFY_FLAGS
import com.btcontract.wallet.Utils.Bytes
import util.Try


object Scripts {
  // Script for an HTLC we send to them
  def scriptPubKeyHtlcSend(ourKey: ECKey, theirKey: ECKey, absTimeout: Long,
                           relTimeout: Long, rHash: Bytes, commitRevoke: Bytes) =

    new ScriptBuilder op OP_SIZE number 32 op OP_EQUALVERIFY /* check data size */ op
      OP_HASH160 op OP_DUP data ripemd160(rHash) op OP_EQUAL /* is it an r-value? */ op
      OP_SWAP data ripemd160(commitRevoke) op OP_EQUAL /* or is it a revocation preimage? */ op
      OP_ADD /* if either r-value or revocation preimage match */ op OP_IF data theirKey.getPubKey op
      OP_ELSE number absTimeout op OP_CHECKLOCKTIMEVERIFY number relTimeout op OP_NOP3 /* OP_CSV */ op
      OP_2DROP data ourKey.getPubKey op OP_ENDIF op OP_CHECKSIG

  // Script for an HTLC we receive from them
  def scriptPubKeyHtlcReceive(ourKey: ECKey, theirKey: ECKey, absTimeout: Long,
                              relTimeout: Long, rHash: Bytes, commitRevoke: Bytes) =

    new ScriptBuilder op OP_SIZE number 32 op OP_EQUALVERIFY /* check data size */ op
      OP_HASH160 op OP_DUP data ripemd160(rHash) op OP_EQUAL /* is it an r-value? */ op
      /* if it's an r-val then I can spend this output in <relTimeout> number of blocks */
      OP_IF number relTimeout op OP_NOP3 /* OP_CSV */ op OP_2DROP data ourKey.getPubKey op
      /* else they can spend output now if they know revocation or after <absTimeout> */
      OP_ELSE data ripemd160(commitRevoke) op OP_EQUAL op OP_NOTIF number absTimeout op
      OP_CHECKLOCKTIMEVERIFY op OP_DROP op OP_ENDIF data theirKey.getPubKey op
      OP_ENDIF op OP_CHECKSIG

  def pay2wsh(s: Script) = new ScriptBuilder op OP_0 data Sha256Hash.hash(s.getProgram)
  def pay2wpkh(pubKey: ECKey) = new ScriptBuilder op OP_0 data pubKey.getPubKeyHash
  def multiSig2of2(ks: ECKey*) = ScriptBuilder.createRedeemScript(2, ks.asJava)

  def redeemPubKey(pubKey: Bytes) = new ScriptBuilder op OP_DUP op OP_HASH160 data ripemd160(pubKey) op OP_EQUALVERIFY op OP_CHECKSIG
  def redeemSecretOrDelay(delayedKey: ECKey, relTimeout: Long, keyIfSecretKnown: ECKey, hashOfSecret: Bytes) = new ScriptBuilder op
    OP_HASH160 data ripemd160(hashOfSecret) op OP_EQUAL op OP_IF data keyIfSecretKnown.getPubKey op OP_ELSE number relTimeout op
    OP_NOP3 /* OP_CSV */ op OP_DROP data delayedKey.getPubKey op OP_ENDIF op OP_CHECKSIG

  def makeAnchorTx(ourCommitPub: ECKey, theirCommitPub: ECKey, amount: Long): (Transaction, Int) = ???

  def makeCommitTx(inputs: Seq[TransactionInput], ourFinalKey: ECKey, theirFinalKey: ECKey, theirDelay: Long,
                   revocationHash: Bytes, commitmentSpec: CommitmentSpec): Transaction = ???

  def makeBreachTx(inputs: Seq[TransactionInput], ourParams: OurChannelParams, theirParams: TheirChannelParams,
                   commitmentSpec: CommitmentSpec, revocationPreimage: Bytes): Transaction = ???

  def makeFinalTx(inputs: Seq[TransactionInput], ourPubkeyScript: Script, theirPubkeyScript: Script,
                  amountUs: Long, amountThem: Long, fee: Long): (Transaction, proto.close_signature) = ???

  def signTx(ourParams: OurChannelParams, theirParams: TheirChannelParams,
             tx: Transaction, anchorAmount: Long): proto.signature = ???

  def addTheirSigAndSignTx(ourParams: OurChannelParams, theirParams: TheirChannelParams,
                           tx: Transaction, anchorAmount: Long, theirSig: proto.signature): Transaction = ???

  // Commit tx tries to spend an anchor output
  def brokenTxCheck(tx: Transaction, anchorOutput: TransactionOutput) =
    Try apply tx.getInput(0).getScriptSig.correctlySpends(tx, 0,
      anchorOutput.getScriptPubKey, ALL_VERIFY_FLAGS)

  def computeFee(feeRate: Long, htlcNum: Int) =
    (338 + 32 * htlcNum) * feeRate / 2000 * 2

  def applyFees(us: Long, them: Long, fee: Long) =
    if (us < fee / 2) 0L -> Math.max(0L, them - fee + us)
    else if (them < fee / 2) Math.max(us - fee + them, 0L) -> 0L
    else (us - fee / 2, them - fee / 2)
}