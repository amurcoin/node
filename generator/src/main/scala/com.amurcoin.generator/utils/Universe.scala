package com.amurcoin.generator.utils

import com.amurcoin.account.PrivateKeyAccount
import com.amurcoin.state.ByteStr

object Universe {
  var AccountsWithBalances: List[(PrivateKeyAccount, Long)] = Nil
  var IssuedAssets: List[ByteStr]                           = Nil
  var Leases: List[ByteStr]                                 = Nil
}
