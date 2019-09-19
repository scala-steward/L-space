package lspace.lgraph.provider.cassandra

import java.util.concurrent.ConcurrentHashMap

import com.datastax.driver.core.SocketOptions
import com.outworkers.phantom.connectors.KeySpaceBuilder
import com.outworkers.phantom.dsl._
import lspace.codec.json.Decoder
import lspace.lgraph.store.{StoreManager, StoreProvider}
import lspace.lgraph.{GraphManager, LGraph}
import monix.eval.Task

import scala.collection.concurrent
import scala.collection.JavaConverters._
import scala.util.Try

object LCassandraStoreProvider {
  def apply[Json](iri: String, host: String, port: Int)(implicit baseEncoder: NativeTypeEncoder.Aux[Json],
                                                        baseDecoder: Decoder.Aux[Json]): LCassandraStoreProvider[Json] =
    new LCassandraStoreProvider(iri, host, port)

  val keySpaceBuilders: concurrent.Map[StoragePoint, KeySpaceBuilder] =
    new ConcurrentHashMap[StoragePoint, KeySpaceBuilder]().asScala
}
class LCassandraStoreProvider[Json](val iri: String, host: String, port: Int)(
    implicit baseEncoder: NativeTypeEncoder.Aux[Json],
    baseDecoder: Decoder.Aux[Json])
    extends StoreProvider {

  private def createBuilder =
    ContactPoint(host, port)
      .withClusterBuilder(
        _.withSocketOptions(
          new SocketOptions()
            .setConnectTimeoutMillis(200000)
            .setReadTimeoutMillis(200000)
        ))
      .noHeartbeat()
  val keyspaceBuilder =
    LCassandraStoreProvider.keySpaceBuilders.getOrElseUpdate(StoragePoint(host, port), createBuilder)

  val space: LKeySpace = LKeySpace(iri)

  private def getOrCreateKeySpace(iri: String) =
    KeySpace(iri.replace('.', '_').replace('-', '_'))
      .ifNotExists()
      .`with`(
        replication eqs SimpleStrategy.replication_factor(1)
      )

  object graph              extends CassandraGraph(keyspaceBuilder.keySpace(getOrCreateKeySpace(space.graph)))
  object dataGraphTables    extends CassandraGraphTables(keyspaceBuilder.keySpace(getOrCreateKeySpace(space.data)))
  object nsGraphTables      extends CassandraGraphTables(keyspaceBuilder.keySpace(getOrCreateKeySpace(space.ns)))
  object nsIndexGraphTables extends CassandraGraphTables(keyspaceBuilder.keySpace(getOrCreateKeySpace(space.nsIndex)))
  object indexGraphTables   extends CassandraGraphTables(keyspaceBuilder.keySpace(getOrCreateKeySpace(space.index)))
//  object indexIndexGraphTables
//      extends CassandraGraphTables(keyspaceBuilder.keySpace(getOrCreateKeySpace(space.indexIndex)))

  def purge(): Task[Unit] =
    for {
      _ <- Task.deferFuture { graph.truncateAsync() }
      _ <- Task.deferFuture { dataGraphTables.truncateAsync() }
      _ <- Task.deferFuture { nsGraphTables.truncateAsync() }
      _ <- Task.deferFuture { nsIndexGraphTables.truncateAsync() }
      _ <- Task.deferFuture { indexGraphTables.truncateAsync() }
//      _ <- Task.deferFuture { indexIndexGraphTables.truncateAsync() }
    } yield ()

  def stateManager[G <: LGraph](graph: G): GraphManager[G]   = new CassandraGraphManager(graph, this.graph)
  def dataManager[G <: LGraph](graph: G): StoreManager[G]    = new CassandraStoreManager(graph, dataGraphTables)
  def nsManager[G <: LGraph](graph: G): StoreManager[G]      = new CassandraStoreManager(graph, nsGraphTables)
  def nsIndexManager[G <: LGraph](graph: G): StoreManager[G] = new CassandraStoreManager(graph, nsIndexGraphTables)
  def indexManager[G <: LGraph](graph: G): StoreManager[G]   = new CassandraStoreManager(graph, indexGraphTables)
//  def indexIndexManager[G <: LGraph](graph: G): StoreManager[G] =
//    new CassandraStoreManager(graph, indexIndexGraphTables)
}
