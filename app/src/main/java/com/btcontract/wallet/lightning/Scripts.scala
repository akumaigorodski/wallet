package com.btcontract.wallet.lightning

import org.bitcoinj.core._
import org.bitcoinj.script.Script
import org.bitcoinj.script.Script.ALL_VERIFY_FLAGS
import com.btcontract.wallet.Utils.Bytes


object Scripts {
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
