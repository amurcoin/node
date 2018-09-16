package com.amurcoin.state.diffs

import com.amurcoin.features.BlockchainFeatures
import com.amurcoin.settings.{FunctionalitySettings, TestFunctionalitySettings}

package object smart {
  val smartEnabledFS: FunctionalitySettings =
    TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures =
        Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.SmartAssets.id -> 0, BlockchainFeatures.DataTransaction.id -> 0))
}
