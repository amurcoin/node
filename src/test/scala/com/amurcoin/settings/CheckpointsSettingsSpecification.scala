package com.amurcoin.settings

import com.typesafe.config.ConfigFactory
import com.amurcoin.state.ByteStr
import org.scalatest.{FlatSpec, Matchers}

class CheckpointsSettingsSpecification extends FlatSpec with Matchers {
  "CheckpointsSettings" should "read values" in {
    val config   = ConfigFactory.parseString("""
        |amurcoin {
        |  checkpoints {
        |    public-key: "BASE58PUBKEY"
        |  }
        |}
      """.stripMargin).resolve()
    val settings = CheckpointsSettings.fromConfig(config)

    settings.publicKey should be(ByteStr.decodeBase58("BASE58PUBKEY").get)
  }
}