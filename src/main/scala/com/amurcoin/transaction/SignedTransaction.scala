package com.amurcoin.transaction

import com.amurcoin.crypto
import com.amurcoin.state.{ByteStr, _}
import monix.eval.Coeval
import play.api.libs.json._

trait SignedTransaction extends ProvenTransaction with Signed {

  protected override def proofField = {
    val sig = JsString(this.signature.base58)
    Seq("signature" -> sig, "proofs" -> JsArray(Seq(sig)))
  }

  val signature: ByteStr

  def proofs: Proofs = Proofs.create(Seq(signature)).explicitGet()

  val signatureValid: Coeval[Boolean] = Coeval.evalOnce(crypto.verify(signature.arr, bodyBytes(), sender.publicKey))
}
