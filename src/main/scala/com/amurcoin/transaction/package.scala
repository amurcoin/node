package com.amurcoin

import com.amurcoin.utils.base58Length
import com.amurcoin.block.{Block, MicroBlock}

package object transaction {

  type AssetId = com.amurcoin.state.ByteStr
  val AssetIdLength: Int       = com.amurcoin.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)
  type DiscardedTransactions = Seq[Transaction]
  type DiscardedBlocks       = Seq[Block]
  type DiscardedMicroBlocks  = Seq[MicroBlock]
  type AuthorizedTransaction = Authorized with Transaction
}
