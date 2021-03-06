package com.amurcoin.it.async

import com.amurcoin.it.api.AsyncHttpApi._
import com.amurcoin.it.{DockerBased, NodeConfigs}
import com.amurcoin.utils.ScorexLogging
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.DurationInt

class NodeApiTestSuite extends FreeSpec with Matchers with DockerBased with ScorexLogging {

  private val nodeConfig = NodeConfigs.newBuilder.withDefault(1).build().head

  "/node/status should report status" in {
    val node = docker.startNode(nodeConfig)
    val f = for {
      status <- node.status
      _ = log.trace(s"#### $status")
      _ = assert(status.blockchainHeight >= status.stateHeight)
    } yield succeed

    Await.ready(f, 2.minute)
  }

}
