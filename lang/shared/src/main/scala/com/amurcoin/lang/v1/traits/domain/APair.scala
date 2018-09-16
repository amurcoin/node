package com.amurcoin.lang.v1.traits.domain

import scodec.bits.ByteVector

case class APair(amountAsset: Option[ByteVector], priceAsset: Option[ByteVector])
