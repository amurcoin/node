package com.amurcoin

import com.amurcoin.settings.WalletSettings
import com.amurcoin.wallet.Wallet

trait TestWallet {
  protected val testWallet = {
    val wallet = Wallet(WalletSettings(None, Some("123"), None))
    wallet.generateNewAccounts(10)
    wallet
  }
}
