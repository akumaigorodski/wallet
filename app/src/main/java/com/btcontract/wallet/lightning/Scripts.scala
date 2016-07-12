package com.btcontract.wallet.lightning

import org.bitcoinj.core._
import org.bitcoinj.script.ScriptOpCodes._
import org.bitcoinj.script.{ScriptBuilder, Script}
import com.btcontract.wallet.helper.Digests.ripemd160
import org.bitcoinj.script.Script.ALL_VERIFY_FLAGS
import com.btcontract.wallet.Utils.Bytes


object Scripts {
  def scriptPubKeyHtlcSend(ourKey: ECKey, theirKey: ECKey, absTimeout: Long,
                           relTimeout: Long, rHash: Bytes, commitRevoke: Bytes) =

    new ScriptBuilder op OP_SIZE number 32 op OP_EQUALVERIFY /* check data size */ op
      OP_HASH160 op OP_DUP data ripemd160(rHash) op OP_EQUAL /* is it an r-value? */ op
      OP_SWAP data ripemd160(commitRevoke) op OP_EQUAL /* or is it a revocation preimage? */ op
      OP_ADD /* if either r-value or revocation preimage match */ op OP_IF data theirKey.getPubKey op
      OP_ELSE number absTimeout op OP_CHECKLOCKTIMEVERIFY number relTimeout op OP_NOP3 /* OP_CLTV */ op
      OP_2DROP data ourKey.getPubKey op OP_ENDIF op OP_CHECKSIG

  def scriptPubKeyHtlcReceive(ourKey: ECKey, theirKey: ECKey, absTimeout: Long,
                              relTimeout: Long, rHash: Bytes, commitRevoke: Bytes) =

    new ScriptBuilder op OP_SIZE number 32 op OP_EQUALVERIFY /* check data size */ op
      OP_HASH160 op OP_DUP data ripemd160(rHash) op OP_EQUAL /* is it an r-value? */ op
      /* if it's an r-value then I can spend this output in <relTimeout> number of blocks */
      OP_IF number relTimeout op OP_NOP3 /* OP_CLTV */ op OP_2DROP data ourKey.getPubKey op
      /* or else they can spend output now if they know revocation or after <absTimeout> */
      OP_ELSE data ripemd160(commitRevoke) op OP_EQUAL op OP_NOTIF number absTimeout op
      OP_CHECKLOCKTIMEVERIFY op OP_DROP op OP_ENDIF op OP_CHECKSIG

  def redeemSecretOrDelay(delayedKey: ECKey, relTimeout: Long, keyIfSecretKnown: ECKey, hashOfSecret: Bytes) =
    new ScriptBuilder op OP_HASH160 data ripemd160(hashOfSecret) op OP_EQUAL op OP_IF data keyIfSecretKnown.getPubKey op
      OP_ELSE number relTimeout op OP_NOP3 /* OP_CLTV */ op OP_DROP data delayedKey.getPubKey op OP_ENDIF op OP_CHECKSIG

  def makeAnchorTx(ourCommitPub: ECKey, theirCommitPub: ECKey, amount: Long): Transaction = ???

  def makeCommitTx(inputs: java.util.List[TransactionInput], ourFinalKey: Bytes, theirFinalKey: Bytes,
                   theirDelay: Int, revocationHash: Bytes, commitmentSpec: CommitmentSpec): Transaction = ???

  def makeFinalTx(inputs: java.util.List[TransactionInput], ourPubkeyScript: Script,
                  theirPubkeyScript: Script, amountUs: Long, amountThem: Long,
                  fee: Long): Transaction = ???

  def signTx(ourParams: OurChannelParams, theirParams: TheirChannelParams,
             anchorAmount: Long, tx: Transaction): proto.signature = ???

  def addTheirSigAndSignTx(ourParams: OurChannelParams, theirParams: TheirChannelParams,
                           anchorAmount: Coin, tx: Transaction, theirSig: proto.signature): Transaction = ???

  // Commit tx tries to spend an anchor output
  def checkSigOrThrow(tx: Transaction, anchorOutput: TransactionOutput) =
    tx.getInput(0).getScriptSig.correctlySpends(tx, 0, anchorOutput.getScriptPubKey, ALL_VERIFY_FLAGS)
}
