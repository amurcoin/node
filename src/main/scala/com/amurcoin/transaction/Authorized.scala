package com.amurcoin.transaction

import com.amurcoin.account.PublicKeyAccount

trait Authorized {
  val sender: PublicKeyAccount
}
