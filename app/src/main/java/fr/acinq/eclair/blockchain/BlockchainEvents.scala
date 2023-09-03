package fr.acinq.eclair.blockchain

import fr.acinq.bitcoin.{Block, Transaction}


sealed trait BlockchainEvent

case class NewBlock(block: Block) extends BlockchainEvent

case class NewTransaction(tx: Transaction) extends BlockchainEvent

case class CurrentBlockCount(blockCount: Long) extends BlockchainEvent
