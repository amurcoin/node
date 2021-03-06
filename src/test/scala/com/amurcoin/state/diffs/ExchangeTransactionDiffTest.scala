package com.amurcoin.state.diffs

import cats.{Order => _, _}
import com.amurcoin.OrderOps._
import com.amurcoin.account.{AddressScheme, PrivateKeyAccount}
import com.amurcoin.features.BlockchainFeatures
import com.amurcoin.lagonaki.mocks.TestBlock
import com.amurcoin.lang.directives.DirectiveParser
import com.amurcoin.lang.v1.ScriptEstimator
import com.amurcoin.lang.v1.compiler.{CompilerContext, CompilerV1}
import com.amurcoin.settings.{Constants, TestFunctionalitySettings}
import com.amurcoin.state._
import com.amurcoin.state.diffs.TransactionDiffer.TransactionValidationError
import com.amurcoin.transaction.ValidationError.AccountBalanceError
import com.amurcoin.transaction.assets.exchange.{Order, _}
import com.amurcoin.transaction.assets.{IssueTransaction, IssueTransactionV1, IssueTransactionV2}
import com.amurcoin.transaction.smart.SetScriptTransaction
import com.amurcoin.transaction.smart.script.v1.ScriptV1
import com.amurcoin.transaction.smart.script.{Script, ScriptCompiler}
import com.amurcoin.transaction.transfer.TransferTransaction
import com.amurcoin.transaction.{GenesisTransaction, Proofs, Transaction, ValidationError}
import com.amurcoin.utils.functionCosts
import com.amurcoin.{NoShrink, TransactionGen, crypto}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Inside, Matchers, PropSpec}

class ExchangeTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with Inside with NoShrink {

  val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id       -> 0,
      BlockchainFeatures.SmartAssets.id         -> 0,
      BlockchainFeatures.SmartAccountsTrades.id -> 0
    )
  )

  property("preserves amurcoin invariant, stores match info, rewards matcher") {

    val preconditionsAndExchange: Gen[(GenesisTransaction, GenesisTransaction, IssueTransaction, IssueTransaction, ExchangeTransaction)] = for {
      buyer  <- accountGen
      seller <- accountGen
      ts     <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
      issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
      maybeAsset1              <- Gen.option(issue1.id())
      maybeAsset2              <- Gen.option(issue2.id()) suchThat (x => x != maybeAsset1)
      exchange                 <- exchangeGeneratorP(buyer, seller, maybeAsset1, maybeAsset2)
    } yield (gen1, gen2, issue1, issue2, exchange)

    forAll(preconditionsAndExchange) {
      case ((gen1, gen2, issue1, issue2, exchange)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(gen1, gen2, issue1, issue2))), TestBlock.create(Seq(exchange)), fs) {
          case (blockDiff, state) =>
            val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.portfolios.values)
            totalPortfolioDiff.balance shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0
            totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

            blockDiff.portfolios(exchange.sender).balance shouldBe exchange.buyMatcherFee + exchange.sellMatcherFee - exchange.fee
        }
    }
  }

  property("buy amurcoin without enough money for fee") {
    val preconditions: Gen[(GenesisTransaction, GenesisTransaction, IssueTransactionV1, ExchangeTransaction)] = for {
      buyer  <- accountGen
      seller <- accountGen
      ts     <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer, 1 * Constants.UnitsInWave, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransactionV1 <- issueGen(buyer)
      exchange <- Gen.oneOf(
        exchangeV1GeneratorP(buyer, seller, None, Some(issue1.id()), fixedMatcherFee = Some(300000)),
        exchangeV2GeneratorP(buyer, seller, None, Some(issue1.id()), fixedMatcherFee = Some(300000))
      )
    } yield {
      (gen1, gen2, issue1, exchange)
    }

    forAll(preconditions) {
      case ((gen1, gen2, issue1, exchange)) =>
        whenever(exchange.amount > 300000) {
          assertDiffAndState(Seq(TestBlock.create(Seq(gen1, gen2, issue1))), TestBlock.create(Seq(exchange)), fs) {
            case (blockDiff, _) =>
              val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.portfolios.values)
              totalPortfolioDiff.balance shouldBe 0
              totalPortfolioDiff.effectiveBalance shouldBe 0
              totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

              blockDiff.portfolios(exchange.sender).balance shouldBe exchange.buyMatcherFee + exchange.sellMatcherFee - exchange.fee
          }
        }
    }
  }

  def createExTx(buy: Order, sell: Order, price: Long, matcher: PrivateKeyAccount, ts: Long): Either[ValidationError, ExchangeTransaction] = {
    val mf     = buy.matcherFee
    val amount = math.min(buy.amount, sell.amount)
    ExchangeTransactionV1.create(
      matcher = matcher,
      buyOrder = buy.asInstanceOf[OrderV1],
      sellOrder = sell.asInstanceOf[OrderV1],
      price = price,
      amount = amount,
      buyMatcherFee = (BigInt(mf) * amount / buy.amount).toLong,
      sellMatcherFee = (BigInt(mf) * amount / sell.amount).toLong,
      fee = buy.matcherFee,
      timestamp = ts
    )
  }

  property("small fee cases") {
    val MatcherFee = 300000L
    val Ts         = 1000L

    val preconditions: Gen[(PrivateKeyAccount, PrivateKeyAccount, PrivateKeyAccount, GenesisTransaction, GenesisTransaction, IssueTransactionV1)] =
      for {
        buyer   <- accountGen
        seller  <- accountGen
        matcher <- accountGen
        ts      <- timestampGen
        gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
        gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
        issue1: IssueTransactionV1 <- issueGen(seller)
      } yield (buyer, seller, matcher, gen1, gen2, issue1)

    forAll(preconditions, priceGen) {
      case ((buyer, seller, matcher, gen1, gen2, issue1), price) =>
        val assetPair = AssetPair(Some(issue1.id()), None)
        val buy       = Order.buy(buyer, matcher, assetPair, price, 1000000L, Ts, Ts + 1, MatcherFee)
        val sell      = Order.sell(seller, matcher, assetPair, price, 1L, Ts, Ts + 1, MatcherFee)
        val tx        = createExTx(buy, sell, price, matcher, Ts).explicitGet()
        assertDiffAndState(Seq(TestBlock.create(Seq(gen1, gen2, issue1))), TestBlock.create(Seq(tx)), fs) {
          case (blockDiff, state) =>
            blockDiff.portfolios(tx.sender).balance shouldBe tx.buyMatcherFee + tx.sellMatcherFee - tx.fee
            state.portfolio(tx.sender).balance shouldBe 0L
        }
    }
  }

  property("Not enough balance") {
    val MatcherFee = 300000L
    val Ts         = 1000L

    val preconditions: Gen[(PrivateKeyAccount, PrivateKeyAccount, PrivateKeyAccount, GenesisTransaction, GenesisTransaction, IssueTransactionV1)] =
      for {
        buyer   <- accountGen
        seller  <- accountGen
        matcher <- accountGen
        ts      <- timestampGen
        gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
        gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
        issue1: IssueTransactionV1 <- issueGen(seller, fixedQuantity = Some(1000L))
      } yield (buyer, seller, matcher, gen1, gen2, issue1)

    forAll(preconditions, priceGen) {
      case ((buyer, seller, matcher, gen1, gen2, issue1), price) =>
        val assetPair = AssetPair(Some(issue1.id()), None)
        val buy       = Order.buy(buyer, matcher, assetPair, price, issue1.quantity + 1, Ts, Ts + 1, MatcherFee)
        val sell      = Order.sell(seller, matcher, assetPair, price, issue1.quantity + 1, Ts, Ts + 1, MatcherFee)
        val tx        = createExTx(buy, sell, price, matcher, Ts).explicitGet()
        assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, issue1))), TestBlock.create(Seq(tx)), fs) { totalDiffEi =>
          inside(totalDiffEi) {
            case Left(TransactionValidationError(AccountBalanceError(errs), _)) =>
              errs should contain key seller.toAddress
          }
        }
    }
  }

  property("Diff for ExchangeTransaction works as expected and doesn't use rounding inside") {
    val MatcherFee = 300000L
    val Ts         = 1000L

    val preconditions: Gen[
      (PrivateKeyAccount, PrivateKeyAccount, PrivateKeyAccount, GenesisTransaction, GenesisTransaction, GenesisTransaction, IssueTransactionV1)] =
      for {
        buyer   <- accountGen
        seller  <- accountGen
        matcher <- accountGen
        ts      <- timestampGen
        gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
        gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
        gen3: GenesisTransaction = GenesisTransaction.create(matcher, ENOUGH_AMT, ts).explicitGet()
        issue1: IssueTransactionV1 <- issueGen(buyer, fixedQuantity = Some(Long.MaxValue))
      } yield (buyer, seller, matcher, gen1, gen2, gen3, issue1)

    val (buyer, seller, matcher, gen1, gen2, gen3, issue1) = preconditions.sample.get
    val assetPair                                          = AssetPair(None, Some(issue1.id()))

    val buy  = Order.buy(buyer, matcher, assetPair, 238, 3100000000L, Ts, Ts + 1, MatcherFee, version = 1: Byte).asInstanceOf[OrderV1]
    val sell = Order.sell(seller, matcher, assetPair, 235, 425532L, Ts, Ts + 1, MatcherFee, version = 1: Byte).asInstanceOf[OrderV1]
    val tx = ExchangeTransactionV1
      .create(
        matcher = matcher,
        buyOrder = buy,
        sellOrder = sell,
        price = 238,
        amount = 425532,
        buyMatcherFee = 41,
        sellMatcherFee = 300000,
        fee = buy.matcherFee,
        timestamp = Ts
      )
      .explicitGet()

    assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, gen3, issue1))), TestBlock.create(Seq(tx))) { totalDiffEi =>
      inside(totalDiffEi) {
        case Right(diff) =>
          import diff.portfolios
          portfolios(buyer).balance shouldBe (-41L + 425532L)
          portfolios(seller).balance shouldBe (-300000L - 425532L)
          portfolios(matcher).balance shouldBe (+41L + 300000L - tx.fee)
      }
    }
  }

  val fsV2 = TestFunctionalitySettings.Enabled
    .copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.SmartAccounts.id       -> 0,
        BlockchainFeatures.SmartAccountsTrades.id -> 0,
        BlockchainFeatures.SmartAssets.id         -> 0,
        BlockchainFeatures.FairPoS.id             -> 0
      ))

  property("ExchangeTransactions valid if all scripts succeeds") {
    val allValidP = smartTradePreconditions(
      scriptGen("Order", true),
      scriptGen("Order", true),
      scriptGen("ExchangeTransaction", true)
    )

    forAll(allValidP) {
      case (genesis, transfers, issueAndScripts, exchangeTx) =>
        val preconBlocks = Seq(
          TestBlock.create(Seq(genesis)),
          TestBlock.create(transfers),
          TestBlock.create(issueAndScripts)
        )
        assertDiffEi(preconBlocks, TestBlock.create(Seq(exchangeTx)), fsV2) { diff =>
          diff.isRight shouldBe true
        }
    }
  }

  property("ExchangeTransactions invalid if buyer scripts fails") {
    val failedOrderScript = smartTradePreconditions(
      scriptGen("Order", false),
      scriptGen("Order", true),
      scriptGen("ExchangeTransaction", true)
    )

    forAll(failedOrderScript) {
      case (genesis, transfers, issueAndScripts, exchangeTx) =>
        val preconBlocks = Seq(TestBlock.create(Seq(genesis)), TestBlock.create(transfers), TestBlock.create(issueAndScripts))
        assertLeft(preconBlocks, TestBlock.create(Seq(exchangeTx)), fsV2)("TransactionNotAllowedByScript")
    }
  }

  property("ExchangeTransactions invalid if seller scripts fails") {
    val failedOrderScript = smartTradePreconditions(
      scriptGen("Order", true),
      scriptGen("Order", false),
      scriptGen("ExchangeTransaction", true)
    )

    forAll(failedOrderScript) {
      case (genesis, transfers, issueAndScripts, exchangeTx) =>
        val preconBlocks = Seq(TestBlock.create(Seq(genesis)), TestBlock.create(transfers), TestBlock.create(issueAndScripts))
        assertLeft(preconBlocks, TestBlock.create(Seq(exchangeTx)), fsV2)("TransactionNotAllowedByScript")
    }
  }

  property("ExchangeTransactions invalid if matcher script fails") {
    val failedMatcherScript = smartTradePreconditions(
      scriptGen("Order", true),
      scriptGen("Order", true),
      scriptGen("ExchangeTransaction", false)
    )

    forAll(failedMatcherScript) {
      case (genesis, transfers, issueAndScripts, exchangeTx) =>
        val preconBlocks = Seq(TestBlock.create(Seq(genesis)), TestBlock.create(transfers), TestBlock.create(issueAndScripts))
        assertLeft(preconBlocks, TestBlock.create(Seq(exchangeTx)), fsV2)("TransactionNotAllowedByScript")
    }
  }

  property("ExchangeTransaction invalid if order signature invalid") {
    val exchangeWithV2Tx =
      simpleTradePreconditions
        .filter(_._5.version == 2)

    forAll(exchangeWithV2Tx) {
      case (gen1, gen2, issue1, issue2, exchange) =>
        val exchangeWithResignedOrder = exchange match {
          case e1 @ ExchangeTransactionV1(bo, so, _, _, _, _, _, _, _) =>
            val newSig = ByteStr(crypto.sign(so.senderPublicKey.publicKey, bo.bodyBytes()))
            e1.copy(buyOrder = bo.updateProofs(Proofs(Seq(newSig))).asInstanceOf[OrderV1])
          case e2 @ ExchangeTransactionV2(bo, so, _, _, _, _, _, _, _) =>
            val newSig = ByteStr(crypto.sign(bo.senderPublicKey.publicKey, so.bodyBytes()))
            e2.copy(sellOrder = so.updateProofs(Proofs(Seq(newSig))))
        }

        val preconBlocks = Seq(
          TestBlock.create(Seq(gen1, gen2)),
          TestBlock.create(Seq(issue1, issue2))
        )

        val blockWithExchange = TestBlock.create(Seq(exchangeWithResignedOrder))

        assertLeft(preconBlocks, blockWithExchange, fs)("Script doesn't exist and proof doesn't validate as signature")
    }
  }

  property("ExchangeTransaction invalid if order contains more than one proofs") {
    val exchangeWithV2Tx =
      simpleTradePreconditions
        .filter(_._5.version == 2)

    def changeOrderProofs(o: Order, newProofs: Proofs): Order = o match {
      case o1 @ OrderV1(_, _, _, _, _, _, _, _, _, _) =>
        o1.copy(proofs = newProofs)
      case o2 @ OrderV2(_, _, _, _, _, _, _, _, _, _) =>
        o2.copy(proofs = newProofs)
    }

    forAll(exchangeWithV2Tx) {
      case (gen1, gen2, issue1, issue2, exchange) =>
        val newProofs = Proofs(
          Seq(
            ByteStr(crypto.sign(exchange.sender.publicKey, exchange.sellOrder.bodyBytes())),
            ByteStr(crypto.sign(exchange.sellOrder.senderPublicKey.publicKey, exchange.sellOrder.bodyBytes()))
          )
        )

        val exchangeWithResignedOrder = exchange match {
          case e1 @ ExchangeTransactionV1(_, so, _, _, _, _, _, _, _) =>
            e1.copy(buyOrder = so.updateProofs(newProofs).asInstanceOf[OrderV1])
          case e2 @ ExchangeTransactionV2(_, so, _, _, _, _, _, _, _) =>
            e2.copy(buyOrder = changeOrderProofs(so, newProofs))
        }

        val preconBlocks = Seq(
          TestBlock.create(Seq(gen1, gen2)),
          TestBlock.create(Seq(issue1, issue2))
        )

        val blockWithExchange = TestBlock.create(Seq(exchangeWithResignedOrder))

        assertLeft(preconBlocks, blockWithExchange, fs)("Script doesn't exist and proof doesn't validate as signature")
    }
  }

  def scriptGen(caseType: String, v: Boolean): String = {
    s"""
       |match tx {
       | case o: $caseType => $v
       | case _ => ${!v}
       |}
      """.stripMargin
  }

  def changeOrderSignature(signWith: Array[Byte], o: Order): Order = {
    lazy val newProofs = Proofs(Seq(ByteStr(crypto.sign(signWith, o.bodyBytes()))))

    o match {
      case o1 @ OrderV1(_, _, _, _, _, _, _, _, _, _) =>
        o1.copy(proofs = newProofs)
      case o2 @ OrderV2(_, _, _, _, _, _, _, _, _, _) =>
        o2.copy(proofs = newProofs)
    }
  }

  def changeTxSignature(signWith: Array[Byte], et: ExchangeTransaction): ExchangeTransaction = {
    lazy val newSignature = ByteStr(crypto.sign(signWith, et.bodyBytes()))
    lazy val newProofs    = Proofs(Seq(newSignature))

    et match {
      case e1 @ ExchangeTransactionV1(_, _, _, _, _, _, _, _, _) =>
        e1.copy(signature = newSignature)

      case e2 @ ExchangeTransactionV2(_, _, _, _, _, _, _, _, _) =>
        e2.copy(proofs = newProofs)
    }
  }

  def compile(scriptText: String, ctx: CompilerContext): Either[String, (Script, Long)] = {
    val compiler = new CompilerV1(ctx)

    val directives = DirectiveParser(scriptText)

    val scriptWithoutDirectives =
      scriptText.lines
        .filter(str => !str.contains("{-#"))
        .mkString("\n")

    for {
      expr       <- compiler.compile(scriptWithoutDirectives, directives)
      script     <- ScriptV1(expr)
      complexity <- ScriptEstimator(functionCosts, expr)
    } yield (script, complexity)
  }

  def smartTradePreconditions(buyerScriptSrc: String,
                              sellerScriptSrc: String,
                              txScript: String): Gen[(GenesisTransaction, List[TransferTransaction], List[Transaction], ExchangeTransaction)] = {
    val enoughFee = 500000

    val txScriptCompiled = ScriptCompiler(txScript).explicitGet()._1

    val sellerScript = Some(ScriptCompiler(sellerScriptSrc).explicitGet()._1)
    val buyerScript  = Some(ScriptCompiler(buyerScriptSrc).explicitGet()._1)

    val chainId = AddressScheme.current.chainId

    for {
      master <- accountGen
      buyer  <- accountGen
      seller <- accountGen
      ts     <- timestampGen
      genesis = GenesisTransaction.create(master, Long.MaxValue, ts).explicitGet()
      tr1     = createAmurcoinTransfer(master, buyer.toAddress, Long.MaxValue / 3, enoughFee, ts + 1).explicitGet()
      tr2     = createAmurcoinTransfer(master, seller.toAddress, Long.MaxValue / 3, enoughFee, ts + 2).explicitGet()
      asset1 = IssueTransactionV2
        .selfSigned(2: Byte, chainId, buyer, "Asset#1".getBytes, "".getBytes, 1000000, 8, false, None, enoughFee, ts + 3)
        .explicitGet()
      asset2 = IssueTransactionV2
        .selfSigned(2: Byte, chainId, seller, "Asset#2".getBytes, "".getBytes, 1000000, 8, false, None, enoughFee, ts + 4)
        .explicitGet()
      setMatcherScript = SetScriptTransaction
        .selfSigned(1: Byte, master, Some(txScriptCompiled), enoughFee, ts + 5)
        .explicitGet()
      setSellerScript = SetScriptTransaction
        .selfSigned(1: Byte, seller, sellerScript, enoughFee, ts + 6)
        .explicitGet()
      setBuyerScript = SetScriptTransaction
        .selfSigned(1: Byte, buyer, buyerScript, enoughFee, ts + 7)
        .explicitGet()
      assetPair = AssetPair(Some(asset1.id()), Some(asset2.id()))
      o1 <- Gen.oneOf(
        OrderV1.buy(seller, master, assetPair, 1000000, 1000000, ts + 8, ts + 10000, enoughFee),
        OrderV2.buy(seller, master, assetPair, 1000000, 1000000, ts + 8, ts + 10000, enoughFee)
      )
      o2 <- Gen.oneOf(
        OrderV1.sell(buyer, master, assetPair, 1000000, 1000000, ts + 9, ts + 10000, enoughFee),
        OrderV2.sell(buyer, master, assetPair, 1000000, 1000000, ts + 9, ts + 10000, enoughFee)
      )
      exchangeTx = {
        ExchangeTransactionV2
          .create(master, o1, o2, 1000000, 1000000, enoughFee, enoughFee, enoughFee, ts + 10)
          .explicitGet()
      }
    } yield (genesis, List(tr1, tr2), List(asset1, asset2, setMatcherScript, setSellerScript, setBuyerScript), exchangeTx)
  }

  val simpleTradePreconditions: Gen[(GenesisTransaction, GenesisTransaction, IssueTransaction, IssueTransaction, ExchangeTransaction)] = for {
    buyer  <- accountGen
    seller <- accountGen
    ts     <- timestampGen
    gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
    gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
    issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
    issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
    maybeAsset1              <- Gen.option(issue1.id())
    maybeAsset2              <- Gen.option(issue2.id()) suchThat (x => x != maybeAsset1)
    exchange                 <- exchangeGeneratorP(buyer, seller, maybeAsset1, maybeAsset2)
  } yield (gen1, gen2, issue1, issue2, exchange)

}
