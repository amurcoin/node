package com.amurcoin.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.amurcoin.it._
import com.amurcoin.it.api.SyncHttpApi._
import com.amurcoin.it.api.SyncMatcherHttpApi._
import com.amurcoin.it.transactions.NodesFromDocker
import com.amurcoin.it.util._
import com.amurcoin.state.ByteStr
import com.amurcoin.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}
import com.amurcoin.it.sync._
import com.amurcoin.lang.v1.compiler.CompilerV1
import com.amurcoin.lang.v1.parser.Parser
import com.amurcoin.transaction.smart.SetScriptTransaction
import com.amurcoin.transaction.smart.script.v1.ScriptV1
import com.amurcoin.utils.dummyCompilerContext
import play.api.libs.json.JsNumber

import scala.concurrent.duration._
import scala.util.Random

class OrdersFromScriptedAccTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker {

  import OrdersFromScriptedAccTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)
  private def bobNode   = nodes(2)

  "issue asset and run test" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceNode.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, 100000000L).id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)
    val aliceAmurcoinPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    // check assets's balances
    aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, someAssetAmount)
    aliceNode.assertAssetBalance(matcherNode.address, aliceAsset, 0)

    "make Bob's address as scripted" in {
      val scriptText = {
        val sc = Parser(s"""true""".stripMargin).get.value
        CompilerV1(dummyCompilerContext, sc).explicitGet()._1
      }

      val script = ScriptV1(scriptText).explicitGet()
      val setScriptTransaction = SetScriptTransaction
        .selfSigned(SetScriptTransaction.supportedVersions.head, bobNode.privateKey, Some(script), minFee, System.currentTimeMillis())
        .right
        .get

      val setScriptId = bobNode
        .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
        .id

      nodes.waitForHeightAriseAndTxPresent(setScriptId)

    }

    "Alice place sell order, but Bob cannot place order, because his acc is scripted" in {
      // Alice places sell order
      val aliceOrder = matcherNode
        .placeOrder(aliceNode, aliceAmurcoinPair, OrderType.SELL, 2.amurcoin * Order.PriceConstant, 500, version = 1, 10.minutes)

      aliceOrder.status shouldBe "OrderAccepted"

      val orderId = aliceOrder.message.id

      // Alice checks that the order in order book
      matcherNode.orderStatus(orderId, aliceAmurcoinPair).status shouldBe "Accepted"
      matcherNode.fullOrderHistory(aliceNode).head.status shouldBe "Accepted"

      // Alice check that order is correct
      val orders = matcherNode.orderBook(aliceAmurcoinPair)
      orders.asks.head.amount shouldBe 500
      orders.asks.head.price shouldBe 2.amurcoin * Order.PriceConstant

      // sell order should be in the aliceNode orderbook
      matcherNode.fullOrderHistory(aliceNode).head.status shouldBe "Accepted"

      // Bob gets error message
      assertBadRequestAndResponse(
        matcherNode
          .placeOrder(bobNode, aliceAmurcoinPair, OrderType.BUY, 2.amurcoin * Order.PriceConstant, 500, version = 1, 10.minutes),
        "Trading on scripted account isn't allowed yet."
      )

    }
  }

}

object OrdersFromScriptedAccTestSuite {
  val ForbiddenAssetId = "FdbnAsset"

  import NodeConfigs.Default

  private val matcherConfig = ConfigFactory.parseString(s"""
                                                           |amurcoin {
                                                           |  matcher {
                                                           |    enable = yes
                                                           |    account = 3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k
                                                           |    bind-address = "0.0.0.0"
                                                           |    order-match-tx-fee = 300000
                                                           |    blacklisted-assets = [$ForbiddenAssetId]
                                                           |    order-cleanup-interval = 20s
                                                           |  }
                                                           |  rest-api {
                                                           |    enable = yes
                                                           |    api-key-hash = 7L6GpLHhA5KyJTAVc8WFHwEcyTY8fC8rRbyMCiFnM4i
                                                           |  }
                                                           |  miner.enable=no
                                                           |}""".stripMargin)

  private val nonGeneratingPeersConfig = ConfigFactory.parseString(
    """amurcoin {
      | matcher.order-cleanup-interval = 30s
      | miner.enable=no
      |}""".stripMargin
  )

  val MatcherFee: Long     = 300000
  val TransactionFee: Long = 300000

  private val Configs: Seq[Config] = {
    val notMatchingNodes = Random.shuffle(Default.init).take(3)
    Seq(matcherConfig.withFallback(Default.last), notMatchingNodes.head) ++
      notMatchingNodes.tail.map(nonGeneratingPeersConfig.withFallback)
  }
}
