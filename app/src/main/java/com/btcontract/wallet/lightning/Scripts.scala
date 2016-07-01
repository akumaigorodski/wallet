package com.btcontract.wallet.lightning

import com.btcontract.wallet.Utils.Bytes
import org.bitcoinj.core.{Transaction, TransactionInput}


object Scripts {
  def makeCommitTx(inputs: java.util.List[TransactionInput], ourFinalKey: Bytes, theirFinalKey: Bytes,
                   theirDelay: Int, revocationHash: Bytes, commitmentSpec: CommitmentSpec): Transaction = ???

  def signTx(ourParams: OurChannelParams, theirParams: TheirChannelParams,
           anchorAmount: Long, tx: Transaction): proto.signature = ???
}
