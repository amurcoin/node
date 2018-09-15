package com.wavesplatform.matcher.model

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.account.Address
import com.wavesplatform.database.{DBExt, RW}
import com.wavesplatform.matcher.api.DBUtils
import com.wavesplatform.matcher.model.Events._
import com.wavesplatform.matcher.model.LimitOrder.{Filled, OrderStatus}
import com.wavesplatform.matcher.{MatcherKeys, MatcherSettings, OrderAssets}
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.state._
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange.Order
import kamon.Kamon
import org.iq80.leveldb.DB

class OrderHistory(db: DB, settings: MatcherSettings) {
  import OrderHistory._
  import com.wavesplatform.matcher.MatcherKeys._

  private val timer               = Kamon.timer("matcher.order-history.impl")
  private val saveOpenVolumeTimer = timer.refine("action" -> "save-open-volume")
  private val saveOrderInfoTimer  = timer.refine("action" -> "save-order-info")
  private val openVolumeTimer     = timer.refine("action" -> "open-volume")

  private def combine(order: Order, curr: Option[OrderInfo], diff: OrderInfoDiff): OrderInfo = curr match {
    case Some(x) =>
      OrderInfo(
        amount = order.amount,
        filled = x.filled + diff.addExecutedAmount.getOrElse(0L),
        canceled = diff.cancelledByUser.getOrElse(x.canceled),
        minAmount = diff.newMinAmount.orElse(x.minAmount),
        remainingFee = x.remainingFee - diff.executedFee.getOrElse(0L),
        unsafeTotalSpend = Some(OrderInfo.safeSum(x.totalSpend(LimitOrder(order)), diff.lastSpend.getOrElse(0L)))
      )
    case None =>
      val executedAmount = diff.addExecutedAmount.getOrElse(0L)
      val remainingFee   = order.matcherFee - diff.executedFee.getOrElse(0L)
      val canceled       = if (curr.isEmpty) diff.cancelledByUser.map(!_) else diff.cancelledByUser

      OrderInfo(
        amount = order.amount,
        filled = executedAmount,
        canceled = canceled.getOrElse(false),
        minAmount = diff.newMinAmount,
        remainingFee = remainingFee,
        unsafeTotalSpend = diff.lastSpend.orElse(Some(0L))
      )
  }

  // TODO ByteStr(
  private def saveOrderInfo(rw: RW, event: Event): Map[Array[Byte], OrderInfoChange] =
    saveOrderInfoTimer.measure(db.readWrite { rw =>
      val orderInfoDiffs = collectChanges(event)

      val updatedInfo = orderInfoDiffs.map {
        case (order, orderInfoDiff) =>
          val orderId  = order.id()
          val orig     = DBUtils.orderInfoOpt(rw, orderId)
          val combined = combine(order, orig, orderInfoDiff)
          val change   = OrderInfoChange(order, orig, combined)
          rw.put(MatcherKeys.orderInfo(orderId), change.updatedInfo)
          orderId -> change
      }

      updatedInfo.toMap
    })

  private def collectChanges(event: Event): Seq[(Order, OrderInfoDiff)] = event match {
    case OrderAdded(lo) =>
      Seq((lo.order, OrderInfoDiff(newMinAmount = Some(lo.minAmountOfAmountAsset))))

    case oe: OrderExecuted =>
      val submitted = oe.submittedExecuted
      val counter   = oe.counterExecuted

      Seq(
        (submitted.order,
         OrderInfoDiff(
           addExecutedAmount = Some(oe.executedAmount),
           executedFee = Some(submitted.fee),
           newMinAmount = Some(submitted.minAmountOfAmountAsset),
           lastSpend = Some(submitted.getSpendAmount)
         )),
        (counter.order,
         OrderInfoDiff(
           addExecutedAmount = Some(oe.executedAmount),
           executedFee = Some(counter.fee),
           newMinAmount = Some(counter.minAmountOfAmountAsset),
           lastSpend = Some(counter.getSpendAmount)
         ))
      )

    case OrderCanceled(lo, unmatchable) =>
      // The order should not have Cancelled status, if it was cancelled by unmatchable amounts
      val canceled = !unmatchable
      // Hack to get the right status
      val newMinAmount = if (canceled) None else Some(lo.order.amount)
      Seq((lo.order, OrderInfoDiff(cancelledByUser = Some(canceled), newMinAmount = newMinAmount)))
  }

  def openVolume(address: Address, assetId: Option[AssetId]): Long =
    openVolumeTimer.measure(db.get(MatcherKeys.openVolume(address, assetId)).getOrElse(0L))

  private def saveOpenVolume(rw: RW, opDiff: Map[Address, OpenPortfolio]): Unit = saveOpenVolumeTimer.measure {
    for ((address, op) <- opDiff) {
      val newAssets = Set.newBuilder[Option[AssetId]]
      for ((assetId, amount) <- op.orders if amount != 0) {
        val k = MatcherKeys.openVolume(address, assetId)
        val newValue = safeSum(amount, rw.get(k) match {
          case None =>
            newAssets += assetId
            0L
          case Some(v) => v
        })

        rw.put(k, Some(newValue))
      }

      val r = newAssets.result()
      if (r.nonEmpty) {
        val k         = openVolumeSeqNr(address)
        val prevSeqNr = rw.get(k)
        for ((assetId, offset) <- r.zipWithIndex) {
          rw.put(openVolumeAsset(address, prevSeqNr + offset + 1), assetId)
        }
        rw.put(k, prevSeqNr + r.size)
      }
    }
  }

  private def saveOrder(rw: RW, order: Order): Unit = rw.put(MatcherKeys.order(order.id()), Some(order))

  def orderAccepted(event: OrderAdded): Unit = db.readWrite { rw =>
    val lo = event.order
    saveOrder(rw, lo.order)

    val updated = saveOrderInfo(rw, event)
    val opDiff  = diff(event, updated)
    saveOpenVolume(rw, opDiff)

    val accepted = updated(lo.order.id())
    if (accepted.origInfo.isEmpty) {
      // for OrderAdded events, updatedInfo contains just one element
      val o         = accepted.order
      val k         = MatcherKeys.addressOrdersSeqNr(o.senderPublicKey)
      val nextSeqNr = rw.get(k) + 1
      rw.put(k, nextSeqNr)
      rw.put(MatcherKeys.addressOrders(o.senderPublicKey, nextSeqNr), Some(OrderAssets(ByteStr(o.id()), lo.spentAsset)))
    }
  }

  def orderExecuted(event: OrderExecuted): Unit = db.readWrite { rw =>
    saveOrder(rw, event.submitted.order)

    val updated = saveOrderInfo(rw, event)
    val opDiff  = diff(event, updated)
    saveOpenVolume(rw, opDiff)

    val submitted = updated(event.submitted.order.id())
    if (submitted.origInfo.isEmpty) {
      import event.submitted.{order => submittedOrder}
      val k         = MatcherKeys.addressOrdersSeqNr(submittedOrder.senderPublicKey)
      val nextSeqNr = rw.get(k) + 1
      rw.put(k, nextSeqNr)
      rw.put(
        MatcherKeys.addressOrders(submittedOrder.senderPublicKey.toAddress, nextSeqNr),
        Some(OrderAssets(ByteStr(submittedOrder.id()), event.submitted.spentAsset))
      )
    }
  }

  def orderCanceled(event: OrderCanceled): Unit = db.readWrite { rw =>
    val updated = saveOrderInfo(rw, event)
    val opDiff  = diff(event, updated)
    saveOpenVolume(rw, opDiff)

    val canceledOrder = event.limitOrder.order
    val canceled      = updated(canceledOrder.id())
    // When the submitted order can't be matched, it cancelled immediately
    if (canceled.origInfo.isEmpty) {
      saveOrder(rw, canceledOrder)
      val owner     = canceledOrder.senderPublicKey.toAddress
      val k         = MatcherKeys.addressOrdersSeqNr(owner)
      val nextSeqNr = rw.get(k) + 1
      rw.put(k, nextSeqNr)
      rw.put(
        MatcherKeys.addressOrders(owner, nextSeqNr),
        Some(OrderAssets(ByteStr(canceledOrder.id()), event.limitOrder.spentAsset))
      )
    }
  }

  def orderInfo(id: Array[Byte]): OrderInfo = DBUtils.orderInfo(db, id)

  def order(id: Array[Byte]): Option[Order] = db.get(MatcherKeys.order(id))

  def deleteOrder(address: Address, orderId: Array[Byte]): Boolean = db.readWrite { rw =>
    DBUtils.orderInfo(rw, orderId).status match {
      case Filled(_) | LimitOrder.Cancelled(_) =>
        rw.delete(MatcherKeys.order(orderId))
        rw.delete(MatcherKeys.orderInfo(orderId))
        true
      case _ =>
        false
    }
  }
}

object OrderHistory {
  import OrderInfo.orderStatusOrdering

  case class OrderInfoChange(order: Order, origInfo: Option[OrderInfo], updatedInfo: OrderInfo)

  object OrderHistoryOrdering extends Ordering[(ByteStr, OrderInfo, Option[Order])] {
    def orderBy(oh: (ByteStr, OrderInfo, Option[Order])): (OrderStatus, Long) = (oh._2.status, -oh._3.map(_.timestamp).getOrElse(0L))

    override def compare(first: (ByteStr, OrderInfo, Option[Order]), second: (ByteStr, OrderInfo, Option[Order])): Int = {
      implicitly[Ordering[(OrderStatus, Long)]].compare(orderBy(first), orderBy(second))
    }
  }

  private case class OrderInfoDiff(addExecutedAmount: Option[Long] = None,
                                   cancelledByUser: Option[Boolean] = None,
                                   newMinAmount: Option[Long] = None,
                                   executedFee: Option[Long] = None,
                                   lastSpend: Option[Long] = None)

  def diff(event: Event, changes: Map[Array[Byte], OrderInfoChange]): Map[Address, OpenPortfolio] = {
    changes.values.foldLeft(Map.empty[Address, OpenPortfolio]) {
      case (r, change) =>
        Monoid.combine(
          r,
          event match {
            case _: OrderCanceled => if (change.origInfo.isEmpty) Map.empty else diffCancel(change)
            case _                => if (change.origInfo.isEmpty) diffAccepted(change) else diffExecuted(change)
          }
        )
    }
  }

  private def diffAccepted(change: OrderInfoChange): Map[Address, OpenPortfolio] = {
    import change.{order, updatedInfo}
    val lo             = LimitOrder(order)
    val maxSpendAmount = lo.getRawSpendAmount
    val remainingSpend = maxSpendAmount - updatedInfo.totalSpend(lo)
    val remainingFee   = if (lo.feeAcc == lo.rcvAcc) math.max(updatedInfo.remainingFee - lo.getReceiveAmount, 0L) else updatedInfo.remainingFee

    Map(
      order.sender.toAddress -> OpenPortfolio(
        Monoid.combine(
          Map(order.getSpendAssetId -> remainingSpend),
          Map(lo.feeAsset           -> remainingFee)
        )
      )
    )
  }

  private def diffExecuted(change: OrderInfoChange): Map[Address, OpenPortfolio] = {
    import change.{order, updatedInfo}
    val prev         = change.origInfo.getOrElse(throw new IllegalStateException("origInfo must be defined"))
    val lo           = LimitOrder(order)
    val changedSpend = prev.totalSpend(lo) - updatedInfo.totalSpend(lo)
    val changedFee   = -releaseFee(order, prev.remainingFee, updatedInfo.remainingFee)

    Map(
      order.sender.toAddress -> OpenPortfolio(
        Monoid.combine(
          Map(order.getSpendAssetId -> changedSpend),
          Map(lo.feeAsset           -> changedFee)
        )
      )
    )
  }

  private def diffCancel(change: OrderInfoChange): Map[Address, OpenPortfolio] = {
    import change.{order, updatedInfo}
    val lo             = LimitOrder(order)
    val maxSpendAmount = lo.getRawSpendAmount
    val remainingSpend = updatedInfo.totalSpend(lo) - maxSpendAmount
    val remainingFee   = -releaseFee(order, updatedInfo.remainingFee, 0)

    Map(
      order.sender.toAddress -> OpenPortfolio(
        Monoid.combine(
          Map(order.getSpendAssetId -> remainingSpend),
          Map(lo.feeAsset           -> remainingFee)
        )
      )
    )
  }

  /**
    * @return How much reserved fee we should return during this update
    */
  private def releaseFee(totalReceiveAmount: Long, matcherFee: Long, prevRemaining: Long, updatedRemaining: Long): Long = {
    val executedBefore = matcherFee - prevRemaining
    val restReserved   = math.max(matcherFee - totalReceiveAmount - executedBefore, 0L)

    val executed = prevRemaining - updatedRemaining
    math.min(executed, restReserved)
  }

  private def releaseFee(order: Order, prevRemaining: Long, updatedRemaining: Long): Long = {
    val lo = LimitOrder(order)
    if (lo.rcvAsset == lo.feeAsset) releaseFee(lo.getReceiveAmount, order.matcherFee, prevRemaining, updatedRemaining)
    else prevRemaining - updatedRemaining
  }

}
