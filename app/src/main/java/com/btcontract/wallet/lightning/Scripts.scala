package com.btcontract.wallet.lightning

import org.bitcoinj.core.{TransactionOutput, Transaction, TransactionInput}
import org.bitcoinj.script.Script.ALL_VERIFY_FLAGS
import com.btcontract.wallet.Utils.Bytes


object Scripts {
  def makeCommitTx(inputs: java.util.List[TransactionInput], ourFinalKey: Bytes, theirFinalKey: Bytes,
                   theirDelay: Int, revocationHash: Bytes, commitmentSpec: CommitmentSpec): Transaction = ???

  def signTx(ourParams: OurChannelParams, theirParams: TheirChannelParams,
           anchorAmount: Long, tx: Transaction): proto.signature = ???

  def addSigs(ourParams: OurChannelParams, theirParams: TheirChannelParams, anchorAmount: Long,
              tx: Transaction, theirSig: proto.signature): Transaction = ???

  // Commit tx tries to spend an anchor output
  def checkSigOrThrow(tx: Transaction, anchorOutput: TransactionOutput) =
    tx.getInput(0).getScriptSig.correctlySpends(tx, 0, anchorOutput.getScriptPubKey, ALL_VERIFY_FLAGS)
}
