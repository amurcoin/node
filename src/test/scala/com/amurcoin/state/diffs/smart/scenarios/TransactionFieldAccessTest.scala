package com.amurcoin.state.diffs.smart.scenarios

import com.amurcoin.lang.v1.compiler.CompilerV1
import com.amurcoin.lang.v1.parser.Parser
import com.amurcoin.state.diffs.smart._
import com.amurcoin.state._
import com.amurcoin.state.diffs.{assertDiffAndState, assertDiffEi, produce}
import com.amurcoin.utils.dummyCompilerContext
import com.amurcoin.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.amurcoin.lagonaki.mocks.TestBlock
import com.amurcoin.transaction.GenesisTransaction
import com.amurcoin.transaction.lease.LeaseTransaction
import com.amurcoin.transaction.smart.SetScriptTransaction
import com.amurcoin.transaction.transfer._

class TransactionFieldAccessTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  private def preconditionsTransferAndLease(
      code: String): Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransactionV2)] = {
    val untyped = Parser(code).get.value
    val typed   = CompilerV1(dummyCompilerContext, untyped).explicitGet()._1
    preconditionsTransferAndLease(typed)
  }

  private val script =
    """
      |
      | match tx {
      | case ttx: TransferTransaction =>
      |       isDefined(ttx.assetId)==false
      |   case other =>
      |       false
      | }
      """.stripMargin

  property("accessing field of transaction without checking its type first results on exception") {
    forAll(preconditionsTransferAndLease(script)) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }
}
