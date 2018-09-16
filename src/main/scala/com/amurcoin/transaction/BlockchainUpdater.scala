package com.amurcoin.transaction

import com.amurcoin.state.ByteStr
import monix.reactive.Observable
import com.amurcoin.block.Block.BlockId
import com.amurcoin.block.{Block, MicroBlock}

trait BlockchainUpdater {
  def processBlock(block: Block): Either[ValidationError, Option[DiscardedTransactions]]

  def processMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit]

  def removeAfter(blockId: ByteStr): Either[ValidationError, DiscardedBlocks]

  def lastBlockInfo: Observable[LastBlockInfo]

  def isLastBlockId(id: ByteStr): Boolean

  def shutdown(): Unit
}

case class LastBlockInfo(id: BlockId, height: Int, score: BigInt, ready: Boolean)
