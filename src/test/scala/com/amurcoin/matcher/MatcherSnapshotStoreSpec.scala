package com.amurcoin.matcher

import java.io.File
import java.nio.file.Files.createTempDirectory

import akka.persistence.snapshot.SnapshotStoreSpec
import com.typesafe.config.ConfigFactory.parseString
import com.amurcoin.TestHelpers.deleteRecursively
import com.amurcoin.settings.loadConfig

class MatcherSnapshotStoreSpec
    extends SnapshotStoreSpec(loadConfig(parseString(s"""amurcoin.matcher.snapshot-store.leveldb-dir = ${createTempDirectory("matcher").toAbsolutePath}
         |akka.actor.allow-java-serialization = on""".stripMargin))) {
  protected override def afterAll(): Unit = {
    super.afterAll()
    deleteRecursively(new File(system.settings.config.getString("amurcoin.matcher.snapshot-store.leveldb-dir")).toPath)
  }
}
