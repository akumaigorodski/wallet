package com.btcontract.wallet.lightning.crypto

import com.btcontract.wallet.Utils.Bytes
import org.bitcoinj.core.Sha256Hash


object ShaChain { me =>
  type Index = Seq[Boolean]
  type MapIndexHash = Map[Index, Bytes]
  case class Node(parent: Option[Node], value: Bytes, height: Int)

  // Each bool represents a move down the tree
  // A binary representation of index as a sequence of 64 booleans
  implicit def moves(index: Long): Index = for (i <- 63 to 0 by -1) yield index.&(1L << i) != 0
  def flip(in: Bytes, index: Int) = in.updated(index / 8, in(index / 8).^(1 << index % 8).toByte)
  def shaChainFromSeed(hash: Bytes, index: Long) = derive(Node(None, hash, 0), index).value
  def derive(node: Node, directions: Index): Node = (node /: directions)(child)

  // False means left, true means right
  def child(node: Node, right: Boolean): Node = {
    def childHash = Sha256Hash hash flip(node.value, 63 - node.height)
    Node(Some(node), if (right) childHash else node.value, node.height + 1)
  }

  // Hashes are supposed to be received in reverse order so
  // we have parent :+ true which we should be able to recompute
  // since its a left node so its hash is the same as its parent's hash
  def checkRecompute(hashes: MapIndexHash, hash: Bytes, index: Index) = {
    val recomputed = child(Node(None, hash, index.length), right = true).value
    getHash(hashes, index :+ true).forall(_ sameElements recomputed)
  }

  def addHash(hashes: MapIndexHash, hash: Bytes, index: Index): MapIndexHash =
    if (index.last) hashes.updated(index, hash) else index dropRight 1 match { case index1 =>
      assert(checkRecompute(hashes, hash, index1), "Hash recomputation check failed")
      addHash(hashes - (index1 :+ false) - (index1 :+ true), hash, index1)
    }

  def getHash(hashes: MapIndexHash, index: Index) =
    hashes.keys collectFirst { case idx if index startsWith idx =>
      derive(Node(None, hashes(idx), idx.length), index drop idx.length).value
    }
}