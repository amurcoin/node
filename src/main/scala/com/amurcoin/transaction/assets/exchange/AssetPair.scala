package com.amurcoin.transaction.assets.exchange

import com.amurcoin.state.ByteStr
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{JsObject, Json}
import com.amurcoin.transaction._
import com.amurcoin.transaction.assets.exchange.Order.assetIdBytes
import com.amurcoin.transaction.assets.exchange.Validation.booleanOperators

import scala.util.{Success, Try}

case class AssetPair(@ApiModelProperty(dataType = "java.lang.String") amountAsset: Option[AssetId],
                     @ApiModelProperty(dataType = "java.lang.String") priceAsset: Option[AssetId]) {
  import AssetPair._
  @ApiModelProperty(hidden = true)
  lazy val priceAssetStr: String = assetIdStr(priceAsset)
  @ApiModelProperty(hidden = true)
  lazy val amountAssetStr: String = assetIdStr(amountAsset)

  override def toString: String = key

  def key: String = amountAssetStr + "-" + priceAssetStr

  def isValid: Validation = (amountAsset != priceAsset) :| "Invalid AssetPair"

  def bytes: Array[Byte] = assetIdBytes(amountAsset) ++ assetIdBytes(priceAsset)

  def json: JsObject = Json.obj(
    "amountAsset" -> amountAsset.map(_.base58),
    "priceAsset"  -> priceAsset.map(_.base58)
  )

  def reverse = AssetPair(priceAsset, amountAsset)
}

object AssetPair {
  val AmurcoinName = "AMURCOIN"

  def assetIdStr(aid: Option[AssetId]): String = aid.fold(AmurcoinName)(_.base58)

  def extractAssetId(a: String): Try[Option[AssetId]] = a match {
    case `AmurcoinName` => Success(None)
    case other       => ByteStr.decodeBase58(other).map(Option(_))
  }

  def createAssetPair(amountAsset: String, priceAsset: String): Try[AssetPair] =
    for {
      a1 <- extractAssetId(amountAsset)
      a2 <- extractAssetId(priceAsset)
    } yield AssetPair(a1, a2)
}
