package com.amurcoin.state.diffs

import com.amurcoin.features.BlockchainFeatures
import com.amurcoin.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.amurcoin.transaction.ValidationError.GenericError
import com.amurcoin.transaction.{CreateAliasTransaction, ValidationError}
import com.amurcoin.features.FeatureProvider._

import scala.util.Right

object CreateAliasTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: CreateAliasTransaction): Either[ValidationError, Diff] =
    if (blockchain.isFeatureActivated(BlockchainFeatures.DataTransaction, height) && !blockchain.canCreateAlias(tx.alias))
      Left(GenericError("Alias already claimed"))
    else
      Right(
        Diff(height = height,
             tx = tx,
             portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
             aliases = Map(tx.alias               -> tx.sender.toAddress)))
}
