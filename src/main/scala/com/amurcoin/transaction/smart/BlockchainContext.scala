package com.amurcoin.transaction.smart

import cats.kernel.Monoid
import com.amurcoin.lang.Global
import com.amurcoin.lang.v1.evaluator.ctx.EvaluationContext
import com.amurcoin.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.amurcoin.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.amurcoin.state._
import com.amurcoin.transaction._
import com.amurcoin.transaction.assets.exchange.Order
import monix.eval.Coeval
import shapeless._

object BlockchainContext {

  private val baseContext = Monoid.combine(PureContext.ctx, CryptoContext.build(Global)).evaluationContext

  type In = Transaction :+: Order :+: CNil
  def build(nByte: Byte, in: Coeval[In], h: Coeval[Int], blockchain: Blockchain): EvaluationContext =
    Monoid.combine(baseContext, WavesContext.build(new WavesEnvironment(nByte, in, h, blockchain)).evaluationContext)
}
