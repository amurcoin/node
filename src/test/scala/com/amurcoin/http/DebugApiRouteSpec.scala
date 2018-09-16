package com.amurcoin.http

import com.amurcoin.TestWallet
import com.amurcoin.settings.WavesSettings
import com.amurcoin.api.http.ApiKeyNotValid

class DebugApiRouteSpec extends RouteSpec("/debug") with RestAPISettingsHelper with TestWallet {
  private val sampleConfig  = com.typesafe.config.ConfigFactory.load()
  private val wavesSettings = WavesSettings.fromConfig(sampleConfig)
  private val configObject  = sampleConfig.root()
  private val route =
    DebugApiRoute(wavesSettings, null, null, null, null, null, null, null, null, null, null, null, null, null, configObject).route

  routePath("/configInfo") - {
    "requires api-key header" in {
      Get(routePath("/configInfo?full=true")) ~> route should produce(ApiKeyNotValid)
      Get(routePath("/configInfo?full=false")) ~> route should produce(ApiKeyNotValid)
    }
  }
}
