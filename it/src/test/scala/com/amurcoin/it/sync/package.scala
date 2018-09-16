package com.amurcoin.it

import com.amurcoin.state.DataEntry
import com.amurcoin.it.util._

package object sync {
  val minFee                     = 0.001.amurcoin
  val leasingFee                 = 0.002.amurcoin
  val smartFee                   = 0.004.amurcoin
  val issueFee                   = 1.amurcoin
  val burnFee                    = 1.amurcoin
  val sponsorFee                 = 1.amurcoin
  val transferAmount             = 10.amurcoin
  val leasingAmount              = transferAmount
  val issueAmount                = transferAmount
  val massTransferFeePerTransfer = 0.0005.amurcoin
  val someAssetAmount            = 100000
  val matcherFee                 = 0.003.amurcoin

  def calcDataFee(data: List[DataEntry[_]]): Long = {
    val dataSize = data.map(_.toBytes.length).sum + 128
    if (dataSize > 1024) {
      minFee * (dataSize / 1024 + 1)
    } else minFee
  }

  def calcMassTransferFee(numberOfRecipients: Int): Long = {
    minFee + massTransferFeePerTransfer * (numberOfRecipients + 1)
  }

  val supportedVersions = List(null, "2") //sign and broadcast use default for V1
}
