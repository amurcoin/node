package com.amurcoin.http

import com.amurcoin.TestWallet
import com.amurcoin.http.ApiMarshallers._
import play.api.libs.json.JsObject
import com.amurcoin.api.http.{ApiKeyNotValid, WalletApiRoute}
import com.amurcoin.utils.Base58

class WalletRouteSpec extends RouteSpec("/wallet") with RestAPISettingsHelper with TestWallet {
  private val route = WalletApiRoute(restAPISettings, testWallet).route

  private val brokenRestApiSettings     = restAPISettings.copy(apiKeyHash = "InvalidAPIKeyHash")
  private val routeWithIncorrectKeyHash = WalletApiRoute(brokenRestApiSettings, testWallet).route

  routePath("/seed") - {
    "requires api-key header" in {
      Get(routePath("/seed")) ~> route should produce(ApiKeyNotValid)
    }

    "returns seed when api-key header is present" in {
      Get(routePath("/seed")) ~> api_key(apiKey) ~> route ~> check {
        (responseAs[JsObject] \ "seed").as[String] shouldEqual Base58.encode(testWallet.seed)
      }
    }

    "doesn't work if invalid api-key-hash was set" in {
      Get(routePath("/seed")) ~> api_key(apiKey) ~> routeWithIncorrectKeyHash should produce(ApiKeyNotValid)
    }
  }
}
