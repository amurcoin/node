package com.amurcoin.http

import com.typesafe.config.ConfigFactory
import com.amurcoin.crypto
import com.amurcoin.settings.RestAPISettings
import com.amurcoin.utils.Base58

trait RestAPISettingsHelper {
  def apiKey: String = "test_api_key"

  lazy val restAPISettings = {
    val keyHash = Base58.encode(crypto.secureHash(apiKey.getBytes()))
    RestAPISettings.fromConfig(
      ConfigFactory
        .parseString(s"amurcoin.rest-api.api-key-hash = $keyHash")
        .withFallback(ConfigFactory.load()))
  }
}
