package com.amurcoin.state

import com.amurcoin.block.Block.BlockId
import com.amurcoin.consensus.nxt.NxtLikeConsensusBlockData

case class BlockMinerInfo(consensus: NxtLikeConsensusBlockData, timestamp: Long, blockId: BlockId)
