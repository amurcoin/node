package com.amurcoin.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.amurcoin.account.PrivateKeyAccount
import com.amurcoin.api.http.assets.SignedIssueV1Request
import com.amurcoin.it.ReportingTestName
import com.amurcoin.it.api.LevelResponse
import com.amurcoin.it.api.SyncHttpApi._
import com.amurcoin.it.api.SyncMatcherHttpApi._
import com.amurcoin.it.sync.CustomFeeTransactionSuite.defaultAssetQuantity
import com.amurcoin.it.transactions.NodesFromDocker
import com.amurcoin.it.util._
import com.amurcoin.transaction.AssetId
import com.amurcoin.transaction.assets.IssueTransactionV1
import com.amurcoin.transaction.assets.exchange.{AssetPair, OrderType}
import com.amurcoin.utils.Base58
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._
import scala.util.Random

class RoundingIssuesTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName {

  import RoundingIssuesTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  Seq(IssueUsdTx, IssueEthTx, IssueBtcTx).map(createSignedIssueRequest).foreach(matcherNode.signedIssue)
  nodes.waitForHeightArise()

  "should correctly fill an order with small amount" in {
    val aliceBalanceBefore = matcherNode.accountBalances(aliceNode.address)._1
    val bobBalanceBefore   = matcherNode.accountBalances(bobNode.address)._1

    val counter   = matcherNode.prepareOrder(aliceNode, wavesUsdPair, OrderType.BUY, 238, 3100000000L)
    val counterId = matcherNode.placeOrder(counter).message.id

    val submitted   = matcherNode.prepareOrder(bobNode, wavesUsdPair, OrderType.SELL, 235, 425532L)
    val submittedId = matcherNode.placeOrder(submitted).message.id

    val filledAmount = 420169L
    matcherNode.waitOrderStatusAndAmount(wavesUsdPair, submittedId, "Filled", Some(filledAmount), 1.minute)
    matcherNode.waitOrderStatusAndAmount(wavesUsdPair, counterId, "PartiallyFilled", Some(filledAmount), 1.minute)

    matcherNode.cancelOrder(aliceNode, wavesUsdPair, Some(counterId))
    val tx = matcherNode.transactionsByOrder(counterId).head

    matcherNode.waitForTransaction(tx.id)
    val rawExchangeTx = matcherNode.rawTransactionInfo(tx.id)

    (rawExchangeTx \ "price").as[Long] shouldBe counter.price
    (rawExchangeTx \ "amount").as[Long] shouldBe filledAmount
    (rawExchangeTx \ "buyMatcherFee").as[Long] shouldBe 40L
    (rawExchangeTx \ "sellMatcherFee").as[Long] shouldBe 296219L

    val aliceBalanceAfter = matcherNode.accountBalances(aliceNode.address)._1
    val bobBalanceAfter   = matcherNode.accountBalances(bobNode.address)._1

    (aliceBalanceAfter - aliceBalanceBefore) shouldBe (-40L + 420169L)
    (bobBalanceAfter - bobBalanceBefore) shouldBe (-296219L - 420169L)
  }

  "reserved balance should not be negative" in {
    val counter   = matcherNode.prepareOrder(aliceNode, ethBtcPair, OrderType.BUY, 31887L, 923431000L)
    val counterId = matcherNode.placeOrder(counter).message.id

    val submitted   = matcherNode.prepareOrder(bobNode, ethBtcPair, OrderType.SELL, 31887L, 223345000L)
    val submittedId = matcherNode.placeOrder(submitted).message.id

    val filledAmount = 223344937L
    matcherNode.waitOrderStatusAndAmount(ethBtcPair, submittedId, "Filled", Some(filledAmount), 1.minute)
    matcherNode.waitOrderStatusAndAmount(ethBtcPair, counterId, "PartiallyFilled", Some(filledAmount), 1.minute)

    withClue("Bob's reserved balance before cancel")(matcherNode.reservedBalance(bobNode) shouldBe empty)

    matcherNode.cancelOrder(aliceNode, ethBtcPair, Some(counterId))
    val tx = matcherNode.transactionsByOrder(counterId).head

    matcherNode.waitForTransaction(tx.id)

    withClue("Alice's reserved balance after cancel")(matcherNode.reservedBalance(aliceNode) shouldBe empty)
  }

  "should correctly fill 2 counter orders" in {
    val counter1 = matcherNode.prepareOrder(bobNode, wavesUsdPair, OrderType.SELL, 60L, 98333333L)
    matcherNode.placeOrder(counter1).message.id

    val counter2   = matcherNode.prepareOrder(bobNode, wavesUsdPair, OrderType.SELL, 70L, 100000000L)
    val counter2Id = matcherNode.placeOrder(counter2).message.id

    val submitted   = matcherNode.prepareOrder(aliceNode, wavesUsdPair, OrderType.BUY, 1000L, 100000000L)
    val submittedId = matcherNode.placeOrder(submitted).message.id

    matcherNode.waitOrderStatusAndAmount(wavesUsdPair, counter2Id, "PartiallyFilled", Some(2857143L), 1.minute)
    matcherNode.waitOrderStatusAndAmount(wavesUsdPair, submittedId, "Filled", Some(99523810L), 1.minute)

    withClue("orderBook check") {
      val ob = matcherNode.orderBook(wavesUsdPair)
      ob.bids shouldBe empty
      ob.asks shouldBe List(LevelResponse(70L, 97142857L)) // = 100000000 - 2857143
    }
  }

}

object RoundingIssuesTestSuite {

  import ConfigFactory._
  import com.amurcoin.it.NodeConfigs._

  private val ForbiddenAssetId = "FdbnAsset"
  private val Decimals: Byte   = 2

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
  private val alicePk   = PrivateKeyAccount.fromSeed(aliceSeed).right.get

  private val bobSeed = _Configs(2).getString("account-seed")
  private val bobPk   = PrivateKeyAccount.fromSeed(bobSeed).right.get

  val IssueUsdTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = alicePk,
      name = "USD-X".getBytes(),
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

  val IssueEthTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = bobPk,
      name = "ETH-X".getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 8,
      reissuable = false,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val EthId: AssetId = IssueEthTx.id()

  val IssueBtcTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = alicePk,
      name = "BTC-X".getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 8,
      reissuable = false,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val BtcId: AssetId = IssueBtcTx.id()

  val wavesUsdPair = AssetPair(
    amountAsset = None,
    priceAsset = Some(UsdId)
  )

  val ethBtcPair = AssetPair(
    amountAsset = Some(EthId),
    priceAsset = Some(BtcId)
  )

  private val updatedMatcherConfig = parseString(s"""
                                                    |waves.matcher {
                                                    |  price-assets = ["$UsdId", "$BtcId", "AMURCOIN"]
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
