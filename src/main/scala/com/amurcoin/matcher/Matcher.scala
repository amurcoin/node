package com.amurcoin.matcher

import java.io.File
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.pattern.gracefulStop
import akka.stream.ActorMaterializer
import com.amurcoin.api.http.CompositeHttpService
import com.amurcoin.db._
import com.amurcoin.matcher.api.MatcherApiRoute
import com.amurcoin.matcher.market.{MatcherActor, MatcherTransactionWriter, OrderHistoryActor}
import com.amurcoin.matcher.model.OrderBook
import com.amurcoin.settings.{BlockchainSettings, RestAPISettings}
import com.amurcoin.state.Blockchain
import com.amurcoin.transaction.assets.exchange.AssetPair
import com.amurcoin.utils.ScorexLogging
import com.amurcoin.utx.UtxPool
import com.amurcoin.wallet.Wallet
import io.netty.channel.group.ChannelGroup

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.runtime.universe._

class Matcher(actorSystem: ActorSystem,
              wallet: Wallet,
              utx: UtxPool,
              allChannels: ChannelGroup,
              blockchain: Blockchain,
              blockchainSettings: BlockchainSettings,
              restAPISettings: RestAPISettings,
              matcherSettings: MatcherSettings)
    extends ScorexLogging {

  private val pairBuilder    = new AssetPairBuilder(matcherSettings, blockchain)
  private val orderBookCache = new ConcurrentHashMap[AssetPair, OrderBook](1000, 0.9f, 10)
  private val orderBooks     = new AtomicReference(Map.empty[AssetPair, ActorRef])

  private def updateOrderBookCache(assetPair: AssetPair)(newSnapshot: OrderBook): Unit = orderBookCache.put(assetPair, newSnapshot)

  lazy val matcherApiRoutes = Seq(
    MatcherApiRoute(
      wallet,
      pairBuilder,
      matcher,
      orderHistory,
      p => Option(orderBooks.get()).flatMap(_.get(p)),
      p => Option(orderBookCache.get(p)),
      txWriter,
      restAPISettings,
      matcherSettings,
      blockchain,
      db
    )
  )

  lazy val matcherApiTypes = Seq(
    typeOf[MatcherApiRoute]
  )

  lazy val matcher: ActorRef = actorSystem.actorOf(
    MatcherActor.props(orderHistory,
                       pairBuilder,
                       orderBooks,
                       updateOrderBookCache,
                       wallet,
                       utx,
                       allChannels,
                       matcherSettings,
                       blockchain,
                       blockchainSettings.functionalitySettings),
    MatcherActor.name
  )

  lazy val db = openDB(matcherSettings.dataDir)

  lazy val orderHistory: ActorRef = actorSystem.actorOf(OrderHistoryActor.props(db, matcherSettings, utx, wallet), OrderHistoryActor.name)

  lazy val txWriter: ActorRef = actorSystem.actorOf(MatcherTransactionWriter.props(db, matcherSettings), MatcherTransactionWriter.name)

  @volatile var matcherServerBinding: ServerBinding = _

  def shutdownMatcher(): Unit = {
    log.info("Shutting down matcher")
    val stopMatcherTimeout = 5.minutes
    Await.result(gracefulStop(matcher, stopMatcherTimeout, MatcherActor.Shutdown), stopMatcherTimeout)
    log.debug("Matcher's actor system has been shut down")
    db.close()
    log.debug("Matcher's database closed")
    Await.result(matcherServerBinding.unbind(), 10.seconds)
    log.info("Matcher shutdown successful")
  }

  private def checkDirectory(directory: File): Unit = if (!directory.exists()) {
    log.error(s"Failed to create directory '${directory.getPath}'")
    sys.exit(1)
  }

  def runMatcher() {
    val journalDir  = new File(matcherSettings.journalDataDir)
    val snapshotDir = new File(matcherSettings.snapshotsDataDir)
    journalDir.mkdirs()
    snapshotDir.mkdirs()

    checkDirectory(journalDir)
    checkDirectory(snapshotDir)

    log.info(s"Starting matcher on: ${matcherSettings.bindAddress}:${matcherSettings.port} ...")

    implicit val as: ActorSystem                 = actorSystem
    implicit val materializer: ActorMaterializer = ActorMaterializer()

    val combinedRoute = CompositeHttpService(actorSystem, matcherApiTypes, matcherApiRoutes, restAPISettings).compositeRoute
    matcherServerBinding = Await.result(Http().bindAndHandle(combinedRoute, matcherSettings.bindAddress, matcherSettings.port), 5.seconds)

    log.info(s"Matcher bound to ${matcherServerBinding.localAddress} ")
  }

}
