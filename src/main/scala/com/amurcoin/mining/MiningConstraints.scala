package com.amurcoin.mining

import cats.data.NonEmptyList
import com.amurcoin.features.BlockchainFeatures
import com.amurcoin.features.FeatureProvider._
import com.amurcoin.settings.MinerSettings
import com.amurcoin.state.Blockchain
import com.amurcoin.block.Block

case class MiningConstraints(total: MiningConstraint, keyBlock: MiningConstraint, micro: MiningConstraint)

object MiningConstraints {
  val MaxScriptRunsInBlock              = 100
  private val ClassicAmountOfTxsInBlock = 100
  private val MaxTxsSizeInBytes         = 1 * 1024 * 1024 // 1 megabyte

  def apply(minerSettings: MinerSettings, blockchain: Blockchain, height: Int): MiningConstraints = {
    val activatedFeatures     = blockchain.activatedFeaturesAt(height)
    val isNgEnabled           = activatedFeatures.contains(BlockchainFeatures.NG.id)
    val isMassTransferEnabled = activatedFeatures.contains(BlockchainFeatures.MassTransfer.id)
    val isScriptEnabled       = activatedFeatures.contains(BlockchainFeatures.SmartAccounts.id)

    val total: MiningConstraint =
      if (isMassTransferEnabled) OneDimensionalMiningConstraint(MaxTxsSizeInBytes, TxEstimators.sizeInBytes)
      else {
        val maxTxs = if (isNgEnabled) Block.MaxTransactionsPerBlockVer3 else ClassicAmountOfTxsInBlock
        OneDimensionalMiningConstraint(maxTxs, TxEstimators.one)
      }

    new MiningConstraints(
      total =
        if (isScriptEnabled)
          MultiDimensionalMiningConstraint(NonEmptyList.of(OneDimensionalMiningConstraint(MaxScriptRunsInBlock, TxEstimators.scriptRunNumber), total))
        else total,
      keyBlock =
        if (isMassTransferEnabled) OneDimensionalMiningConstraint(0, TxEstimators.one)
        else {
          val maxTxsForKeyBlock = if (isNgEnabled) minerSettings.maxTransactionsInKeyBlock else ClassicAmountOfTxsInBlock
          OneDimensionalMiningConstraint(maxTxsForKeyBlock, TxEstimators.one)
        },
      micro =
        if (isNgEnabled) OneDimensionalMiningConstraint(minerSettings.maxTransactionsInMicroBlock, TxEstimators.one)
        else MiningConstraint.Unlimited
    )
  }
}
