package com.amurcoin.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.amurcoin.account.PrivateKeyAccount
import com.amurcoin.api.http.assets.SignedIssueV1Request
import com.amurcoin.it.ReportingTestName
import com.amurcoin.it.api.AssetDecimalsInfo
import com.amurcoin.it.api.SyncHttpApi._
import com.amurcoin.it.api.SyncMatcherHttpApi._
import com.amurcoin.it.sync.CustomFeeTransactionSuite.defaultAssetQuantity
import com.amurcoin.it.sync._
import com.amurcoin.it.transactions.NodesFromDocker
import com.amurcoin.it.util._
import com.amurcoin.transaction.AssetId
import com.amurcoin.transaction.assets.IssueTransactionV1
import com.amurcoin.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.amurcoin.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.amurcoin.utils.Base58
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode
import scala.util.Random

class TradeBalanceAndRoundingTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName {

  import TradeBalanceAndRoundingTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  Seq(IssueUsdTx, IssueWctTx).map(createSignedIssueRequest).foreach(matcherNode.signedIssue)
  nodes.waitForHeightArise()

  "Alice and Bob trade AMURCOIN-USD" - {
    nodes.waitForHeightArise()
    val aliceWavesBalanceBefore = matcherNode.accountBalances(aliceNode.address)._1
    val bobWavesBalanceBefore   = matcherNode.accountBalances(bobNode.address)._1

    val price           = 238
    val buyOrderAmount  = 425532L
    val sellOrderAmount = 3100000000L

    val correctedSellAmount = correctAmount(sellOrderAmount, price)

    val adjustedAmount = receiveAmount(OrderType.BUY, price, buyOrderAmount)
    val adjustedTotal  = receiveAmount(OrderType.SELL, price, buyOrderAmount)

    log.debug(s"correctedSellAmount: $correctedSellAmount, adjustedAmount: $adjustedAmount, adjustedTotal: $adjustedTotal")

    "place usd-waves order" in {
      // Alice wants to sell USD for Waves

      val bobOrder1   = matcherNode.prepareOrder(bobNode, wavesUsdPair, OrderType.SELL, price, sellOrderAmount)
      val bobOrder1Id = matcherNode.placeOrder(bobOrder1).message.id
      matcherNode.waitOrderStatus(wavesUsdPair, bobOrder1Id, "Accepted", 1.minute)
      matcherNode.reservedBalance(bobNode)("AMURCOIN") shouldBe sellOrderAmount + matcherFee
      matcherNode.tradableBalance(bobNode, wavesUsdPair)("AMURCOIN") shouldBe bobWavesBalanceBefore - (sellOrderAmount + matcherFee)

      val aliceOrder   = matcherNode.prepareOrder(aliceNode, wavesUsdPair, OrderType.BUY, price, buyOrderAmount)
      val aliceOrderId = matcherNode.placeOrder(aliceOrder).message.id
      matcherNode.waitOrderStatusAndAmount(wavesUsdPair, aliceOrderId, "Filled", Some(420169L), 1.minute)

      // Bob wants to buy some USD
      matcherNode.waitOrderStatusAndAmount(wavesUsdPair, bobOrder1Id, "PartiallyFilled", Some(420169L), 1.minute)

      // Each side get fair amount of assets
      val exchangeTx = matcherNode.transactionsByOrder(aliceOrder.idStr()).headOption.getOrElse(fail("Expected an exchange transaction"))
      nodes.waitForHeightAriseAndTxPresent(exchangeTx.id)
    }

    "get opened trading markets. USD price-asset " in {
      val openMarkets = matcherNode.tradingMarkets()
      openMarkets.markets.size shouldBe 1
      val markets = openMarkets.markets.head

      markets.amountAssetName shouldBe "AMURCOIN"
      markets.amountAssetInfo shouldBe Some(AssetDecimalsInfo(8))

      markets.priceAssetName shouldBe usdAssetName
      markets.priceAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))
    }

    "check usd and waves balance after fill" in {
      val aliceWavesBalanceAfter = matcherNode.accountBalances(aliceNode.address)._1
      val aliceUsdBalance        = matcherNode.assetBalance(aliceNode.address, UsdId.base58).balance

      val bobWavesBalanceAfter = matcherNode.accountBalances(bobNode.address)._1
      val bobUsdBalance        = matcherNode.assetBalance(bobNode.address, UsdId.base58).balance

      (aliceWavesBalanceAfter - aliceWavesBalanceBefore) should be(
        adjustedAmount - (BigInt(matcherFee) * adjustedAmount / buyOrderAmount).bigInteger.longValue())

      aliceUsdBalance - defaultAssetQuantity should be(-adjustedTotal)
      bobWavesBalanceAfter - bobWavesBalanceBefore should be(
        -adjustedAmount - (BigInt(matcherFee) * adjustedAmount / sellOrderAmount).bigInteger.longValue())
      bobUsdBalance should be(adjustedTotal)
    }

    "check filled amount and tradable balance" in {
      val bobsOrderId  = matcherNode.fullOrderHistory(bobNode).head.id
      val filledAmount = matcherNode.orderStatus(bobsOrderId, wavesUsdPair).filledAmount.getOrElse(0L)

      filledAmount shouldBe adjustedAmount
    }

    "check reserved balance" in {
      val reservedFee = BigInt(matcherFee) - (BigInt(matcherFee) * adjustedAmount / sellOrderAmount)
      log.debug(s"reservedFee: $reservedFee")
      val expectedBobReservedBalance = correctedSellAmount - adjustedAmount + reservedFee
      matcherNode.reservedBalance(bobNode)("AMURCOIN") shouldBe expectedBobReservedBalance

      matcherNode.reservedBalance(aliceNode) shouldBe empty
    }

    "check waves-usd tradable balance" in {
      val expectedBobTradableBalance = bobWavesBalanceBefore - (correctedSellAmount + matcherFee)
      matcherNode.tradableBalance(bobNode, wavesUsdPair)("AMURCOIN") shouldBe expectedBobTradableBalance
      matcherNode.tradableBalance(aliceNode, wavesUsdPair)("AMURCOIN") shouldBe aliceNode.accountBalances(aliceNode.address)._1

      val orderId = matcherNode.fullOrderHistory(bobNode).head.id
      matcherNode.fullOrderHistory(bobNode).size should be(1)
      matcherNode.cancelOrder(bobNode, wavesUsdPair, Some(orderId))
      matcherNode.waitOrderStatus(wavesUsdPair, orderId, "Cancelled", 1.minute)
      matcherNode.tradableBalance(bobNode, wavesUsdPair)("AMURCOIN") shouldBe bobNode.accountBalances(bobNode.address)._1
    }
  }

  "Alice and Bob trade AMURCOIN-USD check CELLING" - {
    val price2           = 289
    val buyOrderAmount2  = 0.07.waves
    val sellOrderAmount2 = 3.waves

    val correctedSellAmount2 = correctAmount(sellOrderAmount2, price2)

    "place usd-waves order" in {
      nodes.waitForHeightArise()
      // Alice wants to sell USD for Waves
      val bobWavesBalanceBefore = matcherNode.accountBalances(bobNode.address)._1
      matcherNode.tradableBalance(bobNode, wavesUsdPair)("AMURCOIN")
      val bobOrder1   = matcherNode.prepareOrder(bobNode, wavesUsdPair, OrderType.SELL, price2, sellOrderAmount2)
      val bobOrder1Id = matcherNode.placeOrder(bobOrder1).message.id
      matcherNode.waitOrderStatus(wavesUsdPair, bobOrder1Id, "Accepted", 1.minute)

      matcherNode.reservedBalance(bobNode)("AMURCOIN") shouldBe correctedSellAmount2 + matcherFee
      matcherNode.tradableBalance(bobNode, wavesUsdPair)("AMURCOIN") shouldBe bobWavesBalanceBefore - (correctedSellAmount2 + matcherFee)

      val aliceOrder   = matcherNode.prepareOrder(aliceNode, wavesUsdPair, OrderType.BUY, price2, buyOrderAmount2)
      val aliceOrderId = matcherNode.placeOrder(aliceOrder).message.id
      matcherNode.waitOrderStatus(wavesUsdPair, aliceOrderId, "Filled", 1.minute)

      // Bob wants to buy some USD
      matcherNode.waitOrderStatus(wavesUsdPair, bobOrder1Id, "PartiallyFilled", 1.minute)

      // Each side get fair amount of assets
      val exchangeTx = matcherNode.transactionsByOrder(aliceOrder.idStr()).headOption.getOrElse(fail("Expected an exchange transaction"))
      nodes.waitForHeightAriseAndTxPresent(exchangeTx.id)
      matcherNode.cancelOrder(bobNode, wavesUsdPair, Some(bobOrder1Id))
    }

  }

  "Alice and Bob trade WCT-USD" - {
    val wctUsdBuyAmount  = 146
    val wctUsdSellAmount = 347
    val wctUsdPrice      = 12739213

    "place wct-usd order" in {
      val aliceUsdBalance = aliceNode.assetBalance(aliceNode.address, UsdId.base58).balance
      val bobUsdBalance   = bobNode.assetBalance(bobNode.address, UsdId.base58).balance

      val bobOrderId = matcherNode.placeOrder(bobNode, wctUsdPair, SELL, wctUsdPrice, wctUsdSellAmount).message.id
      matcherNode.waitOrderStatus(wctUsdPair, bobOrderId, "Accepted", 1.minute)
      val aliceOrderId = matcherNode.placeOrder(aliceNode, wctUsdPair, BUY, wctUsdPrice, wctUsdBuyAmount).message.id
      matcherNode.waitOrderStatus(wctUsdPair, aliceOrderId, "Filled", 1.minute)

      val exchangeTx = matcherNode.transactionsByOrder(aliceOrderId).headOption.getOrElse(fail("Expected an exchange transaction"))
      nodes.waitForHeightAriseAndTxPresent(exchangeTx.id)

      matcherNode.reservedBalance(bobNode)(s"$WctId") should be(wctUsdSellAmount - correctAmount(wctUsdBuyAmount, wctUsdPrice))
      matcherNode.tradableBalance(bobNode, wctUsdPair)(s"$WctId") shouldBe defaultAssetQuantity - wctUsdSellAmount
      matcherNode.tradableBalance(aliceNode, wctUsdPair)(s"$UsdId") shouldBe aliceUsdBalance - receiveAmount(SELL, wctUsdBuyAmount, wctUsdPrice)

      matcherNode.tradableBalance(bobNode, wctUsdPair)(s"$UsdId") shouldBe bobUsdBalance + receiveAmount(SELL, wctUsdBuyAmount, wctUsdPrice)
      matcherNode.reservedBalance(bobNode)("AMURCOIN") shouldBe
        (matcherFee - (BigDecimal(matcherFee * receiveAmount(OrderType.BUY, wctUsdPrice, wctUsdBuyAmount)) / wctUsdSellAmount).toLong)

      matcherNode.cancelOrder(bobNode, wctUsdPair, Some(matcherNode.fullOrderHistory(bobNode).head.id))
    }

  }

  "get opened trading markets. Check WCT-USD" in {
    val openMarkets = matcherNode.tradingMarkets()
    val markets     = openMarkets.markets.last

    markets.amountAssetName shouldBe wctAssetName
    markets.amountAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))

    markets.priceAssetName shouldBe usdAssetName
    markets.priceAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))
  }

  "Alice and Bob trade WCT-AMURCOIN on not enough fee when place order" - {
    val wctWavesSellAmount = 2
    val wctWavesPrice      = 11234560000000L

    "bob lease all waves exact half matcher fee" in {
      val leasingAmount = bobNode.accountBalances(bobNode.address)._1 - leasingFee - matcherFee / 2
      val leaseTxId =
        bobNode.lease(bobNode.address, matcherNode.address, leasingAmount, leasingFee).id
      nodes.waitForHeightAriseAndTxPresent(leaseTxId)
      val bobOrderId = matcherNode.placeOrder(bobNode, wctWavesPair, SELL, wctWavesPrice, wctWavesSellAmount).message.id
      matcherNode.waitOrderStatus(wctWavesPair, bobOrderId, "Accepted", 1.minute)

      matcherNode.tradableBalance(bobNode, wctWavesPair)("AMURCOIN") shouldBe matcherFee / 2 + receiveAmount(SELL, wctWavesPrice, wctWavesSellAmount) - matcherFee
      matcherNode.cancelOrder(bobNode, wctWavesPair, Some(bobOrderId))

      assertBadRequestAndResponse(matcherNode.placeOrder(bobNode, wctWavesPair, SELL, wctWavesPrice, wctWavesSellAmount / 2),
                                  "Not enough tradable balance")

      bobNode.cancelLease(bobNode.address, leaseTxId, leasingFee)
    }
  }

  "Submitted order Canceled during match" in {
    val bobOrder   = matcherNode.prepareOrder(matcherNode, wavesUsdPair, OrderType.SELL, 10L, 10000000L)
    val bobOrderId = matcherNode.placeOrder(bobOrder).message.id
    matcherNode.waitOrderStatus(wavesUsdPair, bobOrderId, "Accepted", 1.minute)

    val aliceOrder   = matcherNode.prepareOrder(aliceNode, wavesUsdPair, OrderType.BUY, 1000L, 100000L)
    val aliceOrderId = matcherNode.placeOrder(aliceOrder).message.id

    matcherNode.waitOrderStatus(wavesUsdPair, aliceOrderId, "Cancelled", 1.minute)

    withClue("Alice's reserved balance:") {
      matcherNode.reservedBalance(aliceNode) shouldBe empty
    }

    val aliceOrders = matcherNode.ordersByAddress(aliceNode, activeOnly = false, 1.minute)
    aliceOrders should not be empty

    val order = aliceOrders.find(_.id == aliceOrderId).getOrElse(throw new IllegalStateException(s"Alice should have the $aliceOrderId order"))
    order.status shouldBe "Cancelled"

    matcherNode.cancelOrder(matcherNode, wavesUsdPair, Some(bobOrderId))
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

object TradeBalanceAndRoundingTestSuite {

  import ConfigFactory._
  import com.amurcoin.it.NodeConfigs._

  private val ForbiddenAssetId = "FdbnAsset"
  val Decimals: Byte           = 2

  private val minerDisabled = parseString("waves.miner.enable = no")
  private val matcherConfig = parseString(s"""
                                             |waves.matcher {
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

  val usdAssetName = "USD-X"
  val wctAssetName = "WCT-X"
  val IssueUsdTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = alicePk,
      name = usdAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueWctTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = bobPk,
      name = wctAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val UsdId: AssetId = IssueUsdTx.id()
  val WctId: AssetId = IssueWctTx.id()

  val wctUsdPair = AssetPair(
    amountAsset = Some(WctId),
    priceAsset = Some(UsdId)
  )

  val wctWavesPair = AssetPair(
    amountAsset = Some(WctId),
    priceAsset = None
  )

  val wavesUsdPair = AssetPair(
    amountAsset = None,
    priceAsset = Some(UsdId)
  )

  private val updatedMatcherConfig = parseString(s"""
                                                    |waves.matcher {
                                                    |  price-assets = ["$UsdId", "AMURCOIN"]
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
