package com.btcontract.wallet.lightning.crypto

import com.btcontract.wallet.Utils.Bytes
import org.bitcoinj.core.Sha256Hash


object ShaChain { me =>
  type Index = Seq[Boolean]
  type MapIndexHash = Map[Index, Bytes]
  type HashesWithLastIndex = (Option[Long], MapIndexHash)
  case class Node(parent: Option[Node], value: Bytes, height: Int)
  val largestIndex = 0xffffffffffffffffl

  // Each bool represents a move down the tree
  // A binary representation of an index as a sequence of 64 booleans
  def moves(index: Long) = for (i <- 63 to 0 by -1) yield index.&(1l << i) != 0
  def flip(in: Bytes, index: Int) = in.updated(index / 8, in(index / 8).^(1 << index % 8).toByte)
  def revIndexFromSeed(hash: Bytes, idx: Long) = derive(Node(None, hash, 0), me moves largestIndex - idx).value
  def revAddHash(hwli: HashesWithLastIndex, hash: Bytes, idx: Long) = addHash(hwli, hash, largestIndex - idx)
  def derive(node: Node, treeDirections: Index) = (node /: treeDirections)(deriveChild)

  // Generate the next node down the tree hierarchy
  def deriveChild(node: Node, right: Boolean) = Node(parent = Some(node), if (right)
    Sha256Hash hash flip(node.value, 63 - node.height) else node.value, node.height + 1)

  // Hashes are supposed to be received in reverse order so
  // we have parent :+ true which we should be able to recompute
  // since a left node's hash is the same as it's parent node's hash
  def checkRecompute(hashes: MapIndexHash, hash: Bytes, idx1: Index) = {
    val check1 = deriveChild(Node(None, hash, idx1.length), right = true).value
    doGetHash(hashes, idx1 :+ true).forall(_ sameElements check1)
  }

  def doAddHash(hashes: MapIndexHash, hash: Bytes, index: Index): MapIndexHash =
    if (index.last) hashes.updated(index, hash) else index dropRight 1 match { case idx =>
      require(checkRecompute(hashes, hash, idx), "Hash recomputation check failed")
      doAddHash(hashes - (idx :+ false) - (idx :+ true), hash, idx)
    }

  def addHash(hwli: HashesWithLastIndex, hash: Bytes, index: Long) = {
    for (lastKnownIndex <- hwli._1) require(index == lastKnownIndex - 1L)
    Some(index) -> doAddHash(hwli._2, hash, me moves index)
  }

  def doGetHash(hashes: MapIndexHash, index: Index) =
    hashes.keys collectFirst { case idx if index startsWith idx =>
      val startingNode = Node(None, hashes(idx), idx.length)
      derive(startingNode, index drop idx.length).value
    }

  def getHash(hwli: HashesWithLastIndex, index: Long) = hwli match {
    case (Some(lastIdx), hs) if index >= lastIdx => doGetHash(hs, me moves index)
    case _ => None
  }

  // Search for a known hash matching a check predicate
  def findHash(hwli: HashesWithLastIndex)(check: Bytes => Boolean) = {
    def doFind(pos: Long): Option[Bytes] = if (pos > largestIndex) None else getHash(hwli, pos) match {
      case correctHash @ Some(generatedHash) if check(generatedHash) => correctHash case _ => doFind(pos + 1)
    }

    // Start from last known
    hwli._1 flatMap doFind
  }
}