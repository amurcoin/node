package com.amurcoin.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.amurcoin.account.PrivateKeyAccount
import com.amurcoin.api.http.assets.SignedIssueV1Request
import com.amurcoin.it.ReportingTestName
import com.amurcoin.it.api.SyncHttpApi._
import com.amurcoin.it.api.SyncMatcherHttpApi._
import com.amurcoin.it.sync.CustomFeeTransactionSuite.defaultAssetQuantity
import com.amurcoin.it.sync.matcherFee
import com.amurcoin.it.transactions.NodesFromDocker
import com.amurcoin.it.util._
import com.amurcoin.transaction.AssetId
import com.amurcoin.transaction.assets.IssueTransactionV1
import com.amurcoin.transaction.assets.exchange.OrderType.BUY
import com.amurcoin.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.amurcoin.utils.Base58
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode
import scala.util.Random

class SeveralPartialOrdersTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName {

  import SeveralPartialOrdersTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
  nodes.waitForHeightArise()

  "Alice and Bob trade AMURCOIN-USD" - {
    nodes.waitForHeightArise()
    val bobWavesBalanceBefore = matcherNode.accountBalances(bobNode.address)._1

    val price           = 238
    val buyOrderAmount  = 425532L
    val sellOrderAmount = 840340L

    "place usd-amurcoin order" in {
      // Alice wants to sell USD for Waves

      val bobOrder   = matcherNode.prepareOrder(bobNode, amurcoinUsdPair, OrderType.SELL, price, sellOrderAmount)
      val bobOrderId = matcherNode.placeOrder(bobOrder).message.id
      matcherNode.waitOrderStatus(amurcoinUsdPair, bobOrderId, "Accepted", 1.minute)
      matcherNode.reservedBalance(bobNode)("AMURCOIN") shouldBe sellOrderAmount + matcherFee
      matcherNode.tradableBalance(bobNode, amurcoinUsdPair)("AMURCOIN") shouldBe bobWavesBalanceBefore - (sellOrderAmount + matcherFee)

      val aliceOrder   = matcherNode.prepareOrder(aliceNode, amurcoinUsdPair, OrderType.BUY, price, buyOrderAmount)
      val aliceOrderId = matcherNode.placeOrder(aliceOrder).message.id
      matcherNode.waitOrderStatus(amurcoinUsdPair, aliceOrderId, "Filled", 1.minute)

      val aliceOrder2   = matcherNode.prepareOrder(aliceNode, amurcoinUsdPair, OrderType.BUY, price, buyOrderAmount)
      val aliceOrder2Id = matcherNode.placeOrder(aliceOrder2).message.id
      matcherNode.waitOrderStatus(amurcoinUsdPair, aliceOrder2Id, "Filled", 1.minute)

      // Bob wants to buy some USD
      matcherNode.waitOrderStatus(amurcoinUsdPair, bobOrderId, "Filled", 1.minute)

      // Each side get fair amount of assets
      val exchangeTx = matcherNode.transactionsByOrder(bobOrder.idStr()).headOption.getOrElse(fail("Expected an exchange transaction"))
      nodes.waitForHeightAriseAndTxPresent(exchangeTx.id)
      matcherNode.reservedBalance(bobNode) shouldBe empty
      matcherNode.reservedBalance(aliceNode) shouldBe empty
    }
  }

  def correctAmount(a: Long, price: Long): Long = {
    val settledTotal = (BigDecimal(price) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
    (BigDecimal(settledTotal) / price * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
  }

  def receiveAmount(ot: OrderType, matchPrice: Long, matchAmount: Long): Long =
    if (ot == BUY) correctAmount(matchAmount, matchPrice)
    else {
      (BigInt(matchAmount) * matchPrice / Order.PriceConstant).bigInteger.longValueExact()
    }

}

object SeveralPartialOrdersTestSuite {

  import ConfigFactory._
  import com.amurcoin.it.NodeConfigs._

  private val ForbiddenAssetId = "FdbnAsset"
  private val Decimals: Byte   = 2

  private val minerDisabled = parseString("amurcoin.miner.enable = no")
  private val matcherConfig = parseString(s"""
                                             |amurcoin.matcher {
                                             |  enable = yes
                                             |  account = 3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k
                                             |  bind-address = "0.0.0.0"
                                             |  order-match-tx-fee = 300000
                                             |  blacklisted-assets = ["$ForbiddenAssetId"]
                                             |  balance-watching.enable = yes
                                             |}""".stripMargin)

  private val _Configs: Seq[Config] = (Default.last +: Random.shuffle(Default.init).take(3))
    .zip(Seq(matcherConfig, minerDisabled, minerDisabled, empty()))
    .map { case (n, o) => o.withFallback(n) }

  private val aliceSeed = _Configs(1).getString("account-seed")
  private val alicePk   = PrivateKeyAccount.fromSeed(aliceSeed).right.get

  val IssueUsdTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = alicePk,
      name = "USD-X".getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      fee = 1.amurcoin,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val UsdId: AssetId = IssueUsdTx.id()

  val amurcoinUsdPair = AssetPair(
    amountAsset = None,
    priceAsset = Some(UsdId)
  )

  private val updatedMatcherConfig = parseString(s"""
                                                    |amurcoin.matcher {
                                                    |  price-assets = [ "$UsdId", "AMURCOIN"]
                                                    |}
     """.stripMargin)

  private val Configs = _Configs.map(updatedMatcherConfig.withFallback(_))

  def createSignedIssueRequest(tx: IssueTransactionV1): SignedIssueV1Request = {
    import tx._
    SignedIssueV1Request(
      Base58.encode(tx.sender.publicKey),
      new String(name),
      new String(description),
      quantity,
      decimals,
      reissuable,
      fee,
      timestamp,
      signature.base58
    )
  }
}
