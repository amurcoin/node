package com.amurcoin.it.sync.transactions

import com.amurcoin.it.api.SyncHttpApi._
import com.amurcoin.it.api.PaymentRequest
import com.amurcoin.it.transactions.BaseTransactionSuite
import com.amurcoin.it.util._
import org.scalatest.prop.TableDrivenPropertyChecks

class PaymentTransactionSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {

  private val paymentAmount = 5.amurcoin
  private val defaulFee     = 1.amurcoin

  test("amurcoin payment changes amurcoin balances and eff.b.") {

    val (firstBalance, firstEffBalance)   = notMiner.accountBalances(firstAddress)
    val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

    val transferId = sender.payment(firstAddress, secondAddress, paymentAmount, defaulFee).id
    nodes.waitForHeightAriseAndTxPresent(transferId)
    notMiner.assertBalances(firstAddress, firstBalance - paymentAmount - defaulFee, firstEffBalance - paymentAmount - defaulFee)
    notMiner.assertBalances(secondAddress, secondBalance + paymentAmount, secondEffBalance + paymentAmount)
  }

  val payment = PaymentRequest(5.amurcoin, 1.amurcoin, firstAddress, secondAddress)
  val endpoints =
    Table("/amurcoin/payment/signature", "/amurcoin/create-signed-payment", "/amurcoin/external-payment", "/amurcoin/broadcast-signed-payment")
  forAll(endpoints) { (endpoint: String) =>
    test(s"obsolete endpoints respond with BadRequest. Endpoint:$endpoint") {
      val errorMessage = "This API is no longer supported"
      assertBadRequestAndMessage(sender.postJson(endpoint, payment), errorMessage)
    }
  }
}
