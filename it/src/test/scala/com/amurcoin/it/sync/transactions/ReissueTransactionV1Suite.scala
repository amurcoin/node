package com.amurcoin.it.sync.transactions

import com.amurcoin.it.api.SyncHttpApi._
import com.amurcoin.it.transactions.BaseTransactionSuite
import com.amurcoin.it.util._
import com.amurcoin.it.sync._

class ReissueTransactionV1Suite extends BaseTransactionSuite {

  test("asset reissue changes issuer's asset balance; issuer's amurcoin balance is decreased by fee") {

    val (balance, effectiveBalance) = notMiner.accountBalances(firstAddress)

    val issuedAssetId = sender.issue(firstAddress, "name2", "description2", someAssetAmount, decimals = 2, reissuable = true, fee = issueFee).id
    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)
    notMiner.assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)

    val reissueTxId = sender.reissue(firstAddress, issuedAssetId, someAssetAmount, reissuable = true, fee = issueFee).id
    nodes.waitForHeightAriseAndTxPresent(reissueTxId)
    notMiner.assertBalances(firstAddress, balance - 2 * issueFee, effectiveBalance - 2 * issueFee)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, 2 * someAssetAmount)
  }

  test("can't reissue not reissuable asset") {
    val (balance, effectiveBalance) = notMiner.accountBalances(firstAddress)

    val issuedAssetId = sender.issue(firstAddress, "name2", "description2", someAssetAmount, decimals = 2, reissuable = false, issueFee).id
    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)
    notMiner.assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)

    assertBadRequestAndMessage(sender.reissue(firstAddress, issuedAssetId, someAssetAmount, reissuable = true, fee = issueFee),
                               "Asset is not reissuable")
    nodes.waitForHeightArise()

    notMiner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)
    notMiner.assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee)
  }

  test("not able to reissue if cannot pay fee - insufficient funds") {

    val (balance, effectiveBalance) = notMiner.accountBalances(firstAddress)
    val reissueFee                  = effectiveBalance + 1.amurcoin

    val issuedAssetId = sender.issue(firstAddress, "name3", "description3", someAssetAmount, decimals = 2, reissuable = true, issueFee).id

    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

    assertBadRequestAndMessage(sender.reissue(firstAddress, issuedAssetId, someAssetAmount, reissuable = true, fee = reissueFee),
                               "negative amurcoin balance")
    nodes.waitForHeightArise()

    notMiner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)
    notMiner.assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee)

  }

}
