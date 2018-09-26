package com.amurcoin.it.sync.smartcontract

import com.amurcoin.account.{AddressScheme, Alias}
import com.amurcoin.it.api.SyncHttpApi._
import com.amurcoin.it.sync.minFee
import com.amurcoin.it.transactions.BaseTransactionSuite
import com.amurcoin.lang.v1.FunctionHeader
import com.amurcoin.lang.v1.compiler.{CompilerV1, Terms}
import com.amurcoin.state._
import com.amurcoin.transaction.CreateAliasTransactionV2
import com.amurcoin.transaction.smart.SetScriptTransaction
import com.amurcoin.transaction.smart.script.v1.ScriptV1
import com.amurcoin.transaction.transfer.TransferTransactionV2
import com.amurcoin.utils.dummyCompilerContext
import org.scalatest.CancelAfterFailure
import play.api.libs.json.JsNumber

class ScriptExecutionErrorSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val acc0 = pkByAddress(firstAddress)
  private val acc1 = pkByAddress(secondAddress)
  private val acc2 = pkByAddress(thirdAddress)
  private val ts   = System.currentTimeMillis()

  test("custom throw message") {
    val scriptSrc =
      """
        |match tx {
        |  case t : TransferTransaction =>
        |    let res = if isDefined(t.assetId) then extract(t.assetId) == base58'' else isDefined(t.assetId) == false
        |    res
        |  case t : SetScriptTransaction => true
        |  case other => throw("Your transaction has incorrect type.")
        |}
      """.stripMargin

    val compiled = ScriptV1(
      new CompilerV1(dummyCompilerContext).compile(scriptSrc, Nil).explicitGet(),
      checkSize = false
    ).explicitGet()

    val tx = sender.signedBroadcast(
      SetScriptTransaction.selfSigned(1, acc2, Some(compiled), minFee, ts).explicitGet().json() +
        ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
    nodes.waitForHeightAriseAndTxPresent(tx.id)

    val alias = Alias.fromString(s"alias:${AddressScheme.current.chainId.toChar}:asdasdasdv").explicitGet()
    assertBadRequestAndResponse(
      sender.signedBroadcast(
        CreateAliasTransactionV2.selfSigned(acc2, 2, alias, minFee, ts).explicitGet().json() +
          ("type" -> JsNumber(CreateAliasTransactionV2.typeId.toInt))),
      "Your transaction has incorrect type."
    )
  }

  test("wrong type of script return value") {
    val script = ScriptV1(
      Terms.FUNCTION_CALL(
        FunctionHeader.Native(100),
        List(Terms.CONST_LONG(3), Terms.CONST_LONG(2))
      )
    ).explicitGet()

    val tx = sender.signAndBroadcast(
      SetScriptTransaction
        .selfSigned(SetScriptTransaction.supportedVersions.head, acc0, Some(script), minFee, ts)
        .explicitGet()
        .json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
    nodes.waitForHeightAriseAndTxPresent(tx.id)

    assertBadRequestAndResponse(
      sender.signedBroadcast(
        TransferTransactionV2
          .selfSigned(2, None, acc0, acc1.toAddress, 1000, ts, None, minFee, Array())
          .explicitGet()
          .json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt))),
      "Probably script does not return boolean"
    )
  }
}