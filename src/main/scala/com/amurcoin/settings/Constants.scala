package com.amurcoin.settings

import com.amurcoin.Version
import com.amurcoin.utils.ScorexLogging

/**
  * System constants here.
  */
object Constants extends ScorexLogging {
  val ApplicationName = "amurcoin"
  val AgentName       = s"Waves v${Version.VersionString}"

  val UnitsInWave = 100000000L
  val TotalWaves  = 100000000L
}
