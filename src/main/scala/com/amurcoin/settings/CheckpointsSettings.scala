package com.amurcoin.settings

import com.typesafe.config.Config
import com.amurcoin.state.ByteStr
import net.ceedubs.ficus.Ficus._

case class CheckpointsSettings(publicKey: ByteStr)

object CheckpointsSettings {
  val configPath: String = "amurcoin.checkpoints"

  def fromConfig(config: Config): CheckpointsSettings = {
    val publicKey = config.as[ByteStr](s"$configPath.public-key")

    CheckpointsSettings(publicKey)
  }
}
