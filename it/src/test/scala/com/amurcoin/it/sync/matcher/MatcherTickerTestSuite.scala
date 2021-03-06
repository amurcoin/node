package com.amurcoin.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.amurcoin.account.PrivateKeyAccount
import com.amurcoin.api.http.assets.SignedIssueV1Request
import com.amurcoin.it.ReportingTestName
import com.amurcoin.it.api.SyncHttpApi._
import com.amurcoin.it.api.SyncMatcherHttpApi
import com.amurcoin.it.api.SyncMatcherHttpApi._
import com.amurcoin.it.sync.CustomFeeTransactionSuite.defaultAssetQuantity
import com.amurcoin.it.transactions.NodesFromDocker
import com.amurcoin.it.util._
import com.amurcoin.transaction.AssetId
import com.amurcoin.transaction.assets.IssueTransactionV1
import com.amurcoin.transaction.assets.exchange.{AssetPair, OrderType}
import com.amurcoin.utils.Base58
import org.scalatest._

import scala.util.Random

class MatcherTickerTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName {

  import MatcherTickerTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
  nodes.waitForHeightArise()

  "matcher ticker validation" - {
    "get tickers for unavailable asset should produce error" in {
      SyncMatcherHttpApi.assertNotFoundAndMessage(matcherNode.marketStatus(wctAmurcoinPair), s"Invalid Asset ID: ${IssueEightDigitAssetTx.id()}")
    }

    "status of empty orderbook" in {
//    TODO: add error message after fix of https://amurcoin.atlassian.net/browse/NODE-1151
//      SyncMatcherHttpApi.assertNotFoundAndMessage(matcherNode.marketStatus(amurcoinUsdPair), s"")
    }

    "error of non-existed order" in {
      //TODO: add error message after fix of https://amurcoin.atlassian.net/browse/NODE-1151
//      SyncMatcherHttpApi.assertNotFoundAndMessage(matcherNode.orderStatus(IssueUsdTx.id().toString, amurcoinUsdPair), s"")
    }

    "try to work with incorrect pair" in {
      val usdAmurcoinPair = AssetPair(
        amountAsset = Some(UsdId),
        priceAsset = None
      )

      assert(
        matcherNode
          .matcherGet(s"/matcher/orderbook/${usdAmurcoinPair.amountAssetStr}/${usdAmurcoinPair.priceAssetStr}/status", statusCode = 301)
          .getHeader("Location")
          .contains(s"AMURCOIN/${usdAmurcoinPair.amountAssetStr}"))

      //TODO: add error message after fix of https://amurcoin.atlassian.net/browse/NODE-1151
//      SyncMatcherHttpApi.assertNotFoundAndMessage(matcherNode.placeOrder(aliceNode, usdAmurcoinPair, OrderType.BUY, 200, 1.amurcoin), "")
    }

    "issue tokens" in {
      matcherNode.signedIssue(createSignedIssueRequest(IssueEightDigitAssetTx))
      nodes.waitForHeightArise()
    }

    val bidPrice  = 200
    val bidAmount = 1.amurcoin
    val askPrice  = 400
    val askAmount = bidAmount / 2

    "place bid order for first pair" in {
      matcherNode.placeOrder(aliceNode, edUsdPair, OrderType.BUY, bidPrice, bidAmount)
      val aliceOrder = matcherNode.placeOrder(aliceNode, edUsdPair, OrderType.BUY, bidPrice, bidAmount).message.id
      matcherNode.waitOrderStatus(edUsdPair, aliceOrder, "Accepted")

      val r = matcherNode.marketStatus(edUsdPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount)
      r.ask shouldBe None
      r.askAmount shouldBe None
    }

    "place ask order for second pair" in {
      matcherNode.placeOrder(bobNode, wctAmurcoinPair, OrderType.SELL, askPrice, askAmount)
      val bobOrder = matcherNode.placeOrder(bobNode, wctAmurcoinPair, OrderType.SELL, askPrice, askAmount).message.id
      matcherNode.waitOrderStatus(wctAmurcoinPair, bobOrder, "Accepted")
      val r = matcherNode.marketStatus(wctAmurcoinPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe None
      r.bidAmount shouldBe None
      r.ask shouldBe Some(askPrice)
      r.askAmount shouldBe Some(2 * askAmount)
    }

    "place ask order for first pair" in {
      matcherNode.placeOrder(bobNode, edUsdPair, OrderType.SELL, askPrice, askAmount)
      val bobOrder = matcherNode.placeOrder(bobNode, edUsdPair, OrderType.SELL, askPrice, askAmount).message.id
      matcherNode.waitOrderStatus(edUsdPair, bobOrder, "Accepted")
      val r = matcherNode.marketStatus(edUsdPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount)
      r.ask shouldBe Some(askPrice)
      r.askAmount shouldBe Some(2 * askAmount)
    }

    "match bid order for first pair" in {
      val bobOrder = matcherNode.placeOrder(bobNode, edUsdPair, OrderType.SELL, bidPrice, askAmount).message.id
      matcherNode.waitOrderStatus(edUsdPair, bobOrder, "Filled")
      val r = matcherNode.marketStatus(edUsdPair)
      r.lastPrice shouldBe Some(bidPrice)
      r.lastSide shouldBe Some("sell")
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount - askAmount)
      r.ask shouldBe Some(askPrice)
      r.askAmount shouldBe Some(2 * askAmount)

      val bobOrder1 = matcherNode.placeOrder(bobNode, edUsdPair, OrderType.SELL, bidPrice, 3 * askAmount).message.id
      matcherNode.waitOrderStatus(edUsdPair, bobOrder1, "Filled")
      val s = matcherNode.marketStatus(edUsdPair)
      s.lastPrice shouldBe Some(bidPrice)
      s.lastSide shouldBe Some("sell")
      s.bid shouldBe None
      s.bidAmount shouldBe None
      s.ask shouldBe Some(askPrice)
      s.askAmount shouldBe Some(2 * askAmount)
    }

    "match ask order for first pair" in {
      val aliceOrder = matcherNode.placeOrder(aliceNode, edUsdPair, OrderType.BUY, askPrice, bidAmount).message.id
      matcherNode.waitOrderStatus(edUsdPair, aliceOrder, "Filled")
      val r = matcherNode.marketStatus(edUsdPair)
      r.lastPrice shouldBe Some(askPrice)
      r.lastSide shouldBe Some("buy")
      r.bid shouldBe None
      r.bidAmount shouldBe None
      r.ask shouldBe None
      r.askAmount shouldBe None
    }

  }

}

object MatcherTickerTestSuite {

  import ConfigFactory._
  import com.amurcoin.it.NodeConfigs._

  private val ForbiddenAssetId = "FdbnAsset"
  val Decimals: Byte           = 2

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
  private val bobSeed   = _Configs(2).getString("account-seed")
  private val alicePk   = PrivateKeyAccount.fromSeed(aliceSeed).right.get
  private val bobPk     = PrivateKeyAccount.fromSeed(bobSeed).right.get

  val usdAssetName             = "USD-X"
  val eightDigitAssetAssetName = "Eight-X"
  val IssueUsdTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = alicePk,
      name = usdAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      fee = 1.amurcoin,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueEightDigitAssetTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = bobPk,
      name = eightDigitAssetAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 8,
      reissuable = false,
      fee = 1.amurcoin,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val UsdId: AssetId    = IssueUsdTx.id()
  val EightDigitAssetId = IssueEightDigitAssetTx.id()

  val edUsdPair = AssetPair(
    amountAsset = Some(EightDigitAssetId),
    priceAsset = Some(UsdId)
  )

  val wctAmurcoinPair = AssetPair(
    amountAsset = Some(EightDigitAssetId),
    priceAsset = None
  )

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
