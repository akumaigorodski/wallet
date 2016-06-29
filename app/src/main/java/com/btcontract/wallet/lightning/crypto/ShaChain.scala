package com.btcontract.wallet.lightning.crypto

import com.btcontract.wallet.Utils.Bytes
import org.bitcoinj.core.Sha256Hash


object ShaChain { me =>
  type Index = Seq[Boolean]
  type MapIndexHash = Map[Index, Bytes]
  type HashesWithLastIndex = (Option[Long], MapIndexHash)
  case class Node(parent: Option[Node], value: Bytes, height: Int)

  // Each bool represents a move down the tree
  // A binary representation of an index as a sequence of 64 booleans
  def moves(index: Long): Index = for (i <- 63 to 0 by -1) yield index.&(1L << i) != 0
  def flip(in: Bytes, index: Int) = in.updated(index / 8, in(index / 8).^(1 << index % 8).toByte)
  def shaChainFromSeed(hash: Bytes, index: Long) = derive(Node(None, hash, 0), me moves index).value
  def derive(node: Node, directions: Index): Node = (node /: directions)(derive)

  // False means left, true means right
  def derive(node: Node, right: Boolean): Node =
    Node(Some(node), if (right) Sha256Hash hash flip(node.value,
      63 - node.height) else node.value, node.height + 1)

  // Hashes are supposed to be received in reverse order so
  // we have parent :+ true which we should be able to recompute
  // since a left node's hash is the same as it's parent node's hash
  def checkRecompute(hashes: MapIndexHash, hash: Bytes, idx1: Index) = {
    val check1 = derive(Node(None, hash, idx1.length), right = true).value
    doGetHash(hashes, idx1 :+ true).forall(_ sameElements check1)
  }

  def doAddHash(hashes: MapIndexHash, hash: Bytes, index: Index): MapIndexHash =
    if (index.last) hashes.updated(index, hash) else index dropRight 1 match { case idx =>
      require(checkRecompute(hashes, hash, idx), "Hash recomputation check failed")
      doAddHash(hashes - (idx :+ false) - (idx :+ true), hash, idx)
    }

  def addHash(hwli: HashesWithLastIndex, hash: Bytes, index: Long) = {
    for (lastKnownIndex <- hwli._1) require(lastKnownIndex == index - 1L)
    Some(index) -> doAddHash(hwli._2, hash, me moves index)
  }

  def doGetHash(hashes: MapIndexHash, index: Index) =
    hashes.keys collectFirst { case idx if index startsWith idx =>
      val startingNode = Node(None, hashes(idx), idx.length)
      derive(startingNode, index drop idx.length).value
    }

  def getHash(hwli: HashesWithLastIndex, index: Long) = hwli match {
    case (Some(idx), hashes) if idx <= index => doGetHash(hashes, me moves index)
    case _ => None
  }
}