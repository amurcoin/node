package com.amurcoin.transaction

trait VersionedTransaction {
  def version: Byte
}
