package com.amurcoin.state.diffs

import com.amurcoin.state._
import com.amurcoin.transaction.{DataTransaction, ValidationError}

object DataTransactionDiff {

  def apply(blockchain: Blockchain, height: Int)(tx: DataTransaction): Either[ValidationError, Diff] = {
    val sender = tx.sender.toAddress
    Right(
      Diff(
        height,
        tx,
        portfolios = Map(sender  -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
        accountData = Map(sender -> AccountDataInfo(tx.data.map(item => item.key -> item).toMap))
      ))
  }
}