package com.amurcoin.lang.impl

import com.amurcoin.lang.v1.traits.DataType
import com.amurcoin.lang.v1.traits.domain.{Ord, Recipient, Tx}
import shapeless.{:+:, CNil}

import scala.scalajs.js.annotation.JSGlobalScope
import scala.scalajs.{js => platform}

@platform.native
@JSGlobalScope
object Environment extends scalajs.js.Object {
  def height: Int       = platform.native
  def networkByte: Byte = platform.native

  def inputEntity: Tx :+: Ord :+: CNil                    = platform.native
  def transactionById(id: Array[Byte]): Option[Tx]        = platform.native
  def transactionHeightById(id: Array[Byte]): Option[Int] = platform.native

  def data(addressBytes: Recipient, key: String, dataType: DataType): Option[Any] = platform.native

  def resolveAddress(name: String): Either[String, Recipient.Address] = platform.native

  def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = platform.native
}
