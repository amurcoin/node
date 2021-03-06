package com.amurcoin.http

import akka.http.scaladsl.server.{Directive, Route}
import com.amurcoin.api.http.assets.TransferV1Request
import com.amurcoin.api.http.{ApiRoute, DiscontinuedApi}
import com.amurcoin.settings.RestAPISettings
import com.amurcoin.transaction.TransactionFactory
import com.amurcoin.utils.Time
import com.amurcoin.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import javax.ws.rs.Path
import com.amurcoin.wallet.Wallet

@Path("/amurcoin")
@Api(value = "amurcoin")
@Deprecated
case class AmurcoinApiRoute(settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, time: Time)
    extends ApiRoute
    with BroadcastRoute {

  override lazy val route = pathPrefix("amurcoin") {
    externalPayment ~ signPayment ~ broadcastSignedPayment ~ payment ~ createdSignedPayment
  }

  @Deprecated
  @Path("/payment")
  @ApiOperation(
    value = "Send payment from wallet. Deprecated: use /assets/transfer instead",
    notes = "Send payment from wallet to another wallet. Each call sends new payment. Deprecated: use /assets/transfer instead",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json"
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.amurcoin.api.http.assets.TransferV1Request",
        defaultValue = "{\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"sender\":\"senderId\",\n\t\"recipient\":\"recipientId\"\n}"
      )
    ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def payment: Route = (path("payment") & post & withAuth) {
    json[TransferV1Request] { payment =>
      doBroadcast(TransactionFactory.transferAssetV1(payment, wallet, time))
    }
  }

  @Deprecated
  @Path("/payment/signature")
  def signPayment: Route = reject(path("payment" / "signature"))

  @Deprecated
  @Path("/create-signed-payment")
  def createdSignedPayment: Route = reject(path("create-signed-payment"))

  @Deprecated
  @Path("/external-payment")
  def externalPayment: Route = reject(path("external-payment"))

  @Deprecated()
  @Path("/broadcast-signed-payment")
  def broadcastSignedPayment: Route = reject(path("broadcast-signed-payment"))

  private def reject(path: Directive[Unit]): Route = (path & post) {
    complete(DiscontinuedApi)
  }
}
