package com.amurcoin

import com.amurcoin.state.Blockchain
import com.amurcoin.transaction.Transaction

package object mining {
  private[mining] def createConstConstraint(maxSize: Long, transactionSize: => Long) = OneDimensionalMiningConstraint(
    maxSize,
    new com.amurcoin.mining.TxEstimators.Fn {
      override def apply(b: Blockchain, t: Transaction) = transactionSize
      override val minEstimate                          = transactionSize
    }
  )
}
