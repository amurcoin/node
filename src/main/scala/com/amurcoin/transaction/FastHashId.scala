package com.amurcoin.transaction

import com.amurcoin.crypto
import com.amurcoin.state.ByteStr
import monix.eval.Coeval

trait FastHashId extends ProvenTransaction {

  val id: Coeval[AssetId] = Coeval.evalOnce(ByteStr(crypto.fastHash(bodyBytes())))
}
