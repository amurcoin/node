package com.amurcoin.generator

import com.amurcoin.transaction.Transaction

trait TransactionGenerator extends Iterator[Iterator[Transaction]] {
  override val hasNext = true
}
