package com.amurcoin.api.http

import akka.http.scaladsl.server.Directive1
import com.amurcoin.state.{Blockchain, ByteStr}
import com.amurcoin.block.Block
import com.amurcoin.transaction.TransactionParsers

trait CommonApiFunctions { this: ApiRoute =>
  protected[api] def withBlock(blockchain: Blockchain, encodedSignature: String): Directive1[Block] =
    if (encodedSignature.length > TransactionParsers.SignatureStringLength) complete(InvalidSignature)
    else {
      ByteStr
        .decodeBase58(encodedSignature)
        .toOption
        .toRight(InvalidSignature)
        .flatMap(s => blockchain.blockById(s).toRight(BlockDoesNotExist)) match {
        case Right(b) => provide(b)
        case Left(e)  => complete(e)
      }
    }
}
