package com.amurcoin.generator

import cats.Show
import com.amurcoin.account.PrivateKeyAccount
import com.amurcoin.generator.utils.Gen
import com.amurcoin.it.util._
import com.amurcoin.state._
import com.amurcoin.transaction.smart.SetScriptTransaction
import com.amurcoin.transaction.smart.script.Script
import com.amurcoin.transaction.transfer.TransferTransactionV2
import com.amurcoin.transaction.{Transaction}

class SetScriptsTransactionGenerator(settings: SetScriptsTransactionGenerator.Settings, val accounts: Seq[PrivateKeyAccount])
    extends TransactionGenerator {

  override def next(): Iterator[Transaction] = {
    generate(settings).toIterator
  }

  private def generate(settings: SetScriptsTransactionGenerator.Settings): Seq[Transaction] = {
    val bank = accounts.head

    val fee = 0.005.waves

    val script: Script = Gen.script(settings.complexity)

    val setScripts = Range(0, settings.scripts).map { _ =>
      SetScriptTransaction.selfSigned(1, bank, Some(script), 1.waves, System.currentTimeMillis()).explicitGet()
    }

    val txs = Range(0, settings.transfers).map { i =>
      TransferTransactionV2
        .selfSigned(2, None, bank, bank, 1.waves - 2 * fee - i, System.currentTimeMillis(), None, fee, Array.emptyByteArray)
        .explicitGet()
    }

    setScripts ++ txs
  }

}

object SetScriptsTransactionGenerator {
  final case class Settings(scripts: Int, transfers: Int, complexity: Boolean) {
    require(scripts > 0)
    require(transfers >= 0)
  }

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._
      s"""
         | set-scripts = ${scripts}
         | transfers = ${transfers}
         | complexity = ${complexity}
      """.stripMargin
    }

  }
}
