package com.amurcoin.transaction

import com.amurcoin.account.Address

case class AssetAcc(account: Address, assetId: Option[AssetId])
