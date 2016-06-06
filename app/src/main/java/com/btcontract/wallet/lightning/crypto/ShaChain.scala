package com.btcontract.wallet.lightning.crypto

import com.btcontract.wallet.Utils.Bytes
import org.bitcoinj.core.Sha256Hash


object ShaChain {
  type HashSeq = Seq[KnownHash]
  case class KnownHash(hash: Bytes, index: Long)
  case class KnownHashes(known: HashSeq, maxIndex: Long)

  def canDerive(start: Long, end: Long) = (~start & end) == 0
  def shaChainFromSeed(seed: Bytes, idx: Long): Bytes = derive(seed, 0xffffffffffffffffL, idx)
  def flip(in: Bytes, idx: Int): Bytes = in.updated(idx / 8, in(idx / 8).^(1 << idx % 8).toByte)

  def derive(seed: Bytes, start: Long, end: Long) : Bytes = {
    assert(canDerive(start, end), s"Can't derive from $start to $end")
    val branchesNumber = start ^ end
    var hash = seed

    for (i <- 63 to 0 by -1) {
      val foo = (branchesNumber >> i) & 1
      if (foo != 0) hash = Sha256Hash hash flip(hash, i)
    }

    hash
  }

  def getHash(chain: KnownHashes, index: Long) = {
    def derivable(known: KnownHash) = canDerive(known.index, index)
    def deriveKnown(known: KnownHash) = derive(known.hash, known.index, index)
    chain.known find derivable map deriveKnown
  }

  def addHash(chain: KnownHashes, hash: Bytes, index: Long) = {
    def updateKnown(known: HashSeq, acc: HashSeq): HashSeq = known match {
      case KnownHash(knownBytes, knownIndex) :: _ if canDerive(index, knownIndex) =>
        assert(derive(hash, index, knownIndex) sameElements knownBytes)
        acc :+ KnownHash(hash, index)

      case Nil => acc :+ KnownHash(hash, index)
      case head :: tail => updateKnown(tail, acc :+ head)
    }

    val fresh = index == 0 && chain.known.isEmpty
    assert(index == chain.maxIndex + 1 | fresh, "Index exists")
    KnownHashes(updateKnown(chain.known, Seq.empty), index)
  }
}