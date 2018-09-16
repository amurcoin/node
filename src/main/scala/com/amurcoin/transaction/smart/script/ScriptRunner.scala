package com.amurcoin.transaction.smart.script

import cats.implicits._
import com.amurcoin.lang.v1.evaluator.EvaluatorV1
import com.amurcoin.lang.v1.evaluator.ctx.EvaluationContext
import com.amurcoin.lang.ExecutionError
import com.amurcoin.state._
import monix.eval.Coeval
import com.amurcoin.account.AddressScheme
import com.amurcoin.transaction.Transaction
import com.amurcoin.transaction.assets.exchange.Order
import com.amurcoin.transaction.smart.BlockchainContext
import shapeless._

object ScriptRunner {

  def apply[A](height: Int,
               in: Transaction :+: Order :+: CNil,
               blockchain: Blockchain,
               script: Script): (EvaluationContext, Either[ExecutionError, A]) =
    script match {
      case Script.Expr(expr) =>
        val ctx = BlockchainContext.build(
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(height),
          blockchain
        )
        EvaluatorV1[A](ctx, expr)

      case _ => (EvaluationContext.empty, "Unsupported script version".asLeft[A])
    }

}
