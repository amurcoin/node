package com.amurcoin.history

import com.amurcoin.database.{Keys, LevelDBWriter, RW}
import com.amurcoin.settings.AmurcoinSettings
import com.amurcoin.state.{BlockchainUpdaterImpl, NG}
import com.amurcoin.transaction.BlockchainUpdater
import com.amurcoin.utils.{ScorexLogging, Time, UnsupportedFeature, forceStopApplication}
import org.iq80.leveldb.DB

object StorageFactory extends ScorexLogging {
  private val StorageVersion = 2

  def apply(settings: AmurcoinSettings, db: DB, time: Time): BlockchainUpdater with NG = {
    checkVersion(db)
    val levelDBWriter = new LevelDBWriter(db, settings.blockchainSettings.functionalitySettings, settings.maxCacheSize)
    new BlockchainUpdaterImpl(levelDBWriter, settings, time)
  }

  private def checkVersion(db: DB) = {
    val rw      = new RW(db)
    val version = rw.get(Keys.version)
    val height  = rw.get(Keys.height)
    if (version != StorageVersion) {
      if (height == 0) {
        // The storage is empty, set current version
        rw.put(Keys.version, StorageVersion)
      } else {
        // Here we've detected that the storage is not empty and doesn't contain version
        log.error(
          s"Storage version $version is not compatible with expected version $StorageVersion! Please, rebuild node's state, use import or sync from scratch.")
        log.error("FOR THIS REASON THE NODE STOPPED AUTOMATICALLY")
        forceStopApplication(UnsupportedFeature)
      }
    }
    rw.close()
  }
}
