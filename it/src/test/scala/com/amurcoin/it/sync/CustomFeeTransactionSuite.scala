package com.amurcoin.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.amurcoin.it.NodeConfigs.Default
import com.amurcoin.it.api.SyncHttpApi._
import com.amurcoin.it.transactions.BaseTransactionSuite
import com.amurcoin.it.util._
import com.amurcoin.state.{EitherExt2, Sponsorship}
import com.amurcoin.utils.Base58
import org.scalatest.CancelAfterFailure
import com.amurcoin.account.PrivateKeyAccount
import com.amurcoin.api.http.assets.SignedIssueV1Request
import com.amurcoin.transaction.assets.IssueTransactionV1

class CustomFeeTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  import CustomFeeTransactionSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private val transferFee = 100000
  private val assetFee    = 1.amurcoin
  private val assetToken  = 100

  test("make transfer with sponsored asset") {
    val (balance1, eff1) = notMiner.accountBalances(senderAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)
    val (balance3, eff3) = notMiner.accountBalances(minerAddress)

    val req           = createSignedIssueRequest(assetTx)
    val issuedAssetId = sender.signedIssue(req).id
    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

    val sponsorAssetId = sender.sponsorAsset(senderAddress, issuedAssetId, assetToken, assetFee).id
    assert(!sponsorAssetId.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(sponsorAssetId)

    val fees = 2 * assetFee
    notMiner.assertBalances(senderAddress, balance1 - fees, eff1 - fees)
    notMiner.assertAssetBalance(senderAddress, issuedAssetId, defaultAssetQuantity)

    // until `feature-check-blocks-period` blocks have been mined, sponsorship does not occur
    val unsponsoredId = sender.transfer(senderAddress, secondAddress, 1, transferFee, Some(issuedAssetId), Some(issuedAssetId)).id
    nodes.waitForHeightAriseAndTxPresent(unsponsoredId)
    notMiner.assertBalances(senderAddress, balance1 - fees, eff1 - fees)
    notMiner.assertBalances(secondAddress, balance2, eff2)
    notMiner.assertBalances(minerAddress, balance3 + fees, eff3 + fees)

    notMiner.assertAssetBalance(senderAddress, issuedAssetId, defaultAssetQuantity - transferFee - 1)
    notMiner.assertAssetBalance(secondAddress, issuedAssetId, 1)
    notMiner.assertAssetBalance(minerAddress, issuedAssetId, transferFee)

    // after `feature-check-blocks-period` asset fees should be sponsored
    nodes.waitForSameBlockHeadesAt(featureCheckBlocksPeriod)
    val sponsoredId = sender.transfer(senderAddress, secondAddress, 1, transferFee, Some(issuedAssetId), Some(issuedAssetId)).id
    nodes.waitForHeightAriseAndTxPresent(sponsoredId)

    val sponsorship = Sponsorship.toAmurcoin(transferFee, assetToken)
    notMiner.assertBalances(senderAddress, balance1 - fees - sponsorship, eff1 - fees - sponsorship)
    notMiner.assertBalances(secondAddress, balance2, eff2)
    notMiner.assertBalances(minerAddress, balance3 + fees + sponsorship, balance3 + fees + sponsorship)

    notMiner.assertAssetBalance(senderAddress, issuedAssetId, defaultAssetQuantity - transferFee - 2)
    notMiner.assertAssetBalance(secondAddress, issuedAssetId, 2)
    notMiner.assertAssetBalance(minerAddress, issuedAssetId, transferFee)
  }

}

object CustomFeeTransactionSuite {
  val minerAddress             = Default(0).getString("address")
  val senderAddress            = Default(3).getString("address")
  val defaultAssetQuantity     = 999999999999l
  val featureCheckBlocksPeriod = 13

  private val seed = Default(3).getString("account-seed")
  private val pk   = PrivateKeyAccount.fromSeed(seed).explicitGet()
  val assetTx = IssueTransactionV1
    .selfSigned(
      sender = pk,
      name = "asset".getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 2,
      reissuable = false,
      fee = 1.amurcoin,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val assetId = assetTx.id()

  private val minerConfig = ConfigFactory.parseString(s"""
      | amurcoin.fees.transfer.$assetId = 100000
      | amurcoin.blockchain.custom.functionality {
      |   feature-check-blocks-period = $featureCheckBlocksPeriod
      |   blocks-for-feature-activation = $featureCheckBlocksPeriod
      |   pre-activated-features = { 7 = 0 }
      |}""".stripMargin)

  private val notMinerConfig = ConfigFactory.parseString("amurcoin.miner.enable=no").withFallback(minerConfig)

  val Configs: Seq[Config] = Seq(
    minerConfig.withFallback(Default.head),
    notMinerConfig.withFallback(Default(1)),
    notMinerConfig.withFallback(Default(2)),
    notMinerConfig.withFallback(Default(3))
  )

  def createSignedIssueRequest(tx: IssueTransactionV1): SignedIssueV1Request = {
    import tx._
    SignedIssueV1Request(
      Base58.encode(tx.sender.publicKey),
      new String(name),
      new String(description),
      quantity,
      decimals,
      reissuable,
      issueFee,
      timestamp,
      signature.base58
    )
  }
}
