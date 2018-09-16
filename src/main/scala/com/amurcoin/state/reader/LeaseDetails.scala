package com.amurcoin.state.reader

import com.amurcoin.account.{AddressOrAlias, PublicKeyAccount}

case class LeaseDetails(sender: PublicKeyAccount, recipient: AddressOrAlias, height: Int, amount: Long, isActive: Boolean)
