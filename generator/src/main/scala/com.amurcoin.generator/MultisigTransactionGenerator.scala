package com.amurcoin.generator
import cats.Show
import com.amurcoin.crypto
import com.amurcoin.generator.utils.Gen
import com.amurcoin.state._
import com.amurcoin.account.PrivateKeyAccount
import com.amurcoin.transaction.smart.SetScriptTransaction
import com.amurcoin.transaction.smart.script.Script
import com.amurcoin.transaction.transfer.TransferTransactionV2
import com.amurcoin.transaction.{Proofs, Transaction}
import com.amurcoin.it.util._
import scala.util.Random

class MultisigTransactionGenerator(settings: MultisigTransactionGenerator.Settings, val accounts: Seq[PrivateKeyAccount])
    extends TransactionGenerator {

  override def next(): Iterator[Transaction] = {
    generate(settings).toIterator
  }

  private def generate(settings: MultisigTransactionGenerator.Settings): Seq[Transaction] = {

    val bank   = accounts.head
    val owners = Seq(createAccount(), accounts(1), createAccount(), accounts(2), createAccount(), accounts(3), createAccount(), createAccount())

    val enoughFee               = 0.005.amurcoin
    val totalAmountOnNewAccount = 1.amurcoin

    val script: Script = Gen.multiSigScript(owners, 3)

    val setScript = SetScriptTransaction.selfSigned(1, bank, Some(script), enoughFee, System.currentTimeMillis()).explicitGet()

    val res = Range(0, settings.transactions).map { i =>
      val tx = TransferTransactionV2
        .create(2,
                None,
                bank,
                owners(1),
                totalAmountOnNewAccount - 2 * enoughFee - i,
                System.currentTimeMillis(),
                None,
                enoughFee,
                Array.emptyByteArray,
                Proofs.empty)
        .explicitGet()
      val signatures = owners.map(crypto.sign(_, tx.bodyBytes())).map(ByteStr(_))
      tx.copy(proofs = Proofs(signatures))
    }

    println(System.currentTimeMillis())
    println(s"${res.length} tx generated")

    if (settings.firstRun) setScript +: res
    else res
  }

  private def createAccount() = {
    val seedBytes = Array.fill(32)(0: Byte)
    Random.nextBytes(seedBytes)
    PrivateKeyAccount(seedBytes)
  }
}

object MultisigTransactionGenerator {
  final case class Settings(transactions: Int, firstRun: Boolean)

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      s"""
        | transactions = ${x.transactions}
        | firstRun = ${x.firstRun}
      """.stripMargin
    }
  }
}
