package com.amurcoin.history

import com.amurcoin.crypto
import com.amurcoin.db.{CheckpointCodec, PropertiesStorage, SubStorage}
import com.amurcoin.network.Checkpoint
import com.amurcoin.settings.CheckpointsSettings
import org.iq80.leveldb.DB
import com.amurcoin.transaction.ValidationError.GenericError
import com.amurcoin.transaction.{CheckpointService, ValidationError}

class CheckpointServiceImpl(db: DB, settings: CheckpointsSettings)
    extends SubStorage(db, "checkpoints")
    with PropertiesStorage
    with CheckpointService {

  private val CheckpointProperty = "checkpoint"

  override def get: Option[Checkpoint] = getProperty(CheckpointProperty).flatMap(b => CheckpointCodec.decode(b).toOption.map(r => r.value))

  override def set(cp: Checkpoint): Either[ValidationError, Unit] =
    for {
      _ <- Either.cond(!get.forall(_.signature sameElements cp.signature), (), GenericError("Checkpoint already applied"))
      _ <- Either.cond(
        crypto.verify(cp.signature, cp.toSign, settings.publicKey.arr),
        putProperty(CheckpointProperty, CheckpointCodec.encode(cp), None),
        GenericError("Invalid checkpoint signature")
      )
    } yield ()

}
