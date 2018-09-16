package com.amurcoin.matcher.model

import cats.implicits._
import com.amurcoin.account.PublicKeyAccount
import com.amurcoin.matcher.MatcherSettings
import com.amurcoin.matcher.market.OrderBookActor.CancelOrder
import com.amurcoin.matcher.model.Events.OrderAdded
import com.amurcoin.matcher.model.OrderHistory.OrderInfoChange
import com.amurcoin.metrics.TimerExt
import com.amurcoin.state._
import com.amurcoin.transaction.AssetAcc
import com.amurcoin.transaction.ValidationError.GenericError
import com.amurcoin.transaction.assets.exchange.Validation.booleanOperators
import com.amurcoin.transaction.assets.exchange.{Order, Validation}
import com.amurcoin.transaction.smart.Verifier
import com.amurcoin.utils.NTP
import com.amurcoin.utx.UtxPool
import com.amurcoin.wallet.Wallet
import kamon.Kamon

trait OrderValidator {
  val orderHistory: OrderHistory
  val utxPool: UtxPool
  val settings: MatcherSettings
  val wallet: Wallet

  lazy val matcherPubKey: PublicKeyAccount = wallet.findPrivateKey(settings.account).explicitGet()
  val MinExpiration: Long                  = 60 * 1000L

  private val timer = Kamon.timer("matcher.validation")

  private def isBalanceWithOpenOrdersEnough(order: Order): Validation = {
    val lo = LimitOrder(order)

    val b: Map[Option[ByteStr], Long] = Seq(lo.spentAcc, lo.feeAcc).map(a => a.assetId -> spendableBalance(a)).toMap

    val change = OrderInfoChange(lo.order, None, OrderInfo(order.amount, 0L, canceled = false, None, order.matcherFee, Some(0L)))
    val newOrder = OrderHistory
      .diff(OrderAdded(lo), Map(lo.order.id() -> change))
      .getOrElse(order.senderPublicKey.toAddress, OpenPortfolio.empty)

    val open  = b.keySet.map(id => id -> orderHistory.openVolume(order.senderPublicKey, id)).toMap
    val needs = OpenPortfolio(open).combine(newOrder)

    val res: Boolean = b.combine(needs.orders.mapValues(-_)).forall(_._2 >= 0)

    res :| s"Not enough tradable balance: ${b.combine(open.mapValues(-_))}, needs: $newOrder"
  }

  def getTradableBalance(acc: AssetAcc): Long = timer.refine("action" -> "tradableBalance").measure {
    math.max(0l, spendableBalance(acc) - orderHistory.openVolume(acc.account, acc.assetId))
  }

  def validateNewOrder(order: Order): Either[GenericError, Order] =
    timer
      .refine("action" -> "place", "pair" -> order.assetPair.toString)
      .measure {
        val orderSignatureVerification =
          Verifier
            .verifyAsEllipticCurveSignature(order)
            .map(_ => ())
            .leftMap(_.toString)

        val v =
          (order.matcherPublicKey == matcherPubKey) :| "Incorrect matcher public key" &&
            (order.expiration > NTP.correctedTime() + MinExpiration) :| "Order expiration should be > 1 min" &&
            orderSignatureVerification &&
            order.isValid(NTP.correctedTime()) &&
            (order.matcherFee >= settings.minOrderFee) :| s"Order matcherFee should be >= ${settings.minOrderFee}" &&
            (orderHistory.orderInfo(order.id()).status == LimitOrder.NotFound) :| "Order is already accepted" &&
            isBalanceWithOpenOrdersEnough(order)
        Either.cond(v, order, GenericError(v.messages()))
      }

  def validateCancelOrder(cancel: CancelOrder): Either[GenericError, CancelOrder] = {
    timer
      .refine("action" -> "cancel", "pair" -> cancel.assetPair.toString)
      .measure {
        val status = orderHistory.orderInfo(cancel.orderId.arr).status
        val v = status match {
          case LimitOrder.NotFound  => Validation.failure("Order not found")
          case LimitOrder.Filled(_) => Validation.failure("Order is already Filled")
          case _ =>
            orderHistory
              .order(cancel.orderId.arr)
              .fold(false)(_.senderPublicKey == cancel.sender) :| "Order not found"
        }

        Either.cond(v, cancel, GenericError(v.messages()))
      }
  }

  private def spendableBalance(a: AssetAcc): Long = {
    val portfolio = utxPool.portfolio(a.account)
    a.assetId match {
      case Some(x) => portfolio.assets.getOrElse(x, 0)
      case None    => portfolio.spendableBalance
    }
  }
}
