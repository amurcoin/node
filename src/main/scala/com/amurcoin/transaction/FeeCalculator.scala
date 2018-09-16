package com.amurcoin.transaction

import com.amurcoin.settings.FunctionalitySettings
import com.amurcoin.state._
import com.amurcoin.transaction.FeeCalculator._
import com.amurcoin.transaction.ValidationError.InsufficientFee
import com.amurcoin.transaction.assets._
import com.amurcoin.transaction.assets.exchange.ExchangeTransaction
import com.amurcoin.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.amurcoin.transaction.smart.SetScriptTransaction
import com.amurcoin.transaction.transfer._

class FeeCalculator(blockchain: Blockchain) {

  private val Kb = 1024

  def enoughFee[T <: Transaction](tx: T, blockchain: Blockchain, fs: FunctionalitySettings): Either[ValidationError, T] =
    if (blockchain.height >= Sponsorship.sponsoredFeesSwitchHeight(blockchain, fs)) Right(tx)
    else enoughFee(tx)

  def enoughFee[T <: Transaction](tx: T): Either[ValidationError, T] = {
    val (txFeeAssetId, txFeeValue) = tx.assetFee
    val minFeeForTx                = minFeeFor(tx)
    txFeeAssetId match {
      case None =>
        Either
          .cond(
            txFeeValue >= minFeeForTx,
            tx,
            InsufficientFee(s"Fee for ${tx.builder.classTag} transaction does not exceed minimal value of $minFeeForTx")
          )
      case Some(_) => Right(tx)
    }
  }

  private def minFeeFor(tx: Transaction): Long = {
    val baseFee = FeeConstants(tx.builder.typeId)
    tx match {
      case tx: DataTransaction =>
        val sizeInKb = 1 + (tx.bytes().length - 1) / Kb
        baseFee * sizeInKb
      case tx: MassTransferTransaction =>
        val transferFee = FeeConstants(TransferTransactionV1.typeId)
        transferFee + baseFee * tx.transfers.size
      case _ => baseFee
    }
  }
}

object FeeCalculator {
  val FeeConstants = Map(
    PaymentTransaction.typeId      -> 100000,
    IssueTransaction.typeId        -> 100000000,
    TransferTransaction.typeId     -> 100000,
    MassTransferTransaction.typeId -> 50000,
    ReissueTransaction.typeId      -> 100000,
    BurnTransaction.typeId         -> 100000,
    ExchangeTransaction.typeId     -> 300000,
    LeaseTransaction.typeId        -> 100000,
    LeaseCancelTransaction.typeId  -> 100000,
    CreateAliasTransaction.typeId  -> 100000,
    DataTransaction.typeId         -> 100000,
    SetScriptTransaction.typeId    -> 100000,
    SponsorFeeTransaction.typeId   -> 100000000
  )
}
