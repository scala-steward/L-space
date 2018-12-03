package lspace.lgraph.provider.cassandra

import com.datastax.driver.core.SocketOptions
import com.outworkers.phantom.dsl._
import lspace.lgraph.LGraph
import lspace.lgraph.provider.elasticsearch.ESIndexProvider
import lspace.librarian.process.computer.GraphComputerSpec
import lspace.librarian.structure.{Graph, GraphSpec, NodeSpec}
import lspace.librarian.structure.Property.default._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class CassandraStoreManagerSpec extends GraphSpec with NodeSpec with GraphComputerSpec {
  val keySpaceBuilder =
    ContactPoint.local
      .withClusterBuilder(
        _.withSocketOptions(
          new SocketOptions()
            .setConnectTimeoutMillis(20000)
            .setReadTimeoutMillis(20000)
        ))
      .noHeartbeat()

  val store = LCassandraStoreProvider("CassandraStorageManagerSpec", keySpaceBuilder)
  store.deleteAll()
  val sampleStore = LCassandraStoreProvider("CassandraStorageManagerSpec-sample", keySpaceBuilder)
  sampleStore.deleteAll()

  val graph: LGraph =
    LGraph(store, new ESIndexProvider)
  val sampleGraph: LGraph =
    LGraph(sampleStore, new ESIndexProvider)
  def createGraph(iri: String): Graph = {
    val storage = LCassandraStoreProvider(iri, keySpaceBuilder)
    storage.deleteAll()
    LGraph(storage, new ESIndexProvider)
  }

  override def beforeAll: Unit = {
    super.beforeAll
  }

  override def afterAll(): Unit = {
    graph.close()
    sampleGraph.close()
    super.afterAll()
  }

  "CassandraStoreManagerSpec" should {
    "get 10,000 times from store" ignore {
      val start = java.time.Instant.now().toEpochMilli
      val id    = sampleGraph.nodes.hasIri("place-san_jose_de_maipo").head.id
      (1 to 10000).foreach(_ => sampleGraph.nodes.hasId(id))
      val end      = java.time.Instant.now().toEpochMilli
      val duration = end - start
      println(s"get 10,000 times from store took ${duration} milli-seconds")
    }
    "create 10,000 nodes with an iri" ignore {
      val start       = java.time.Instant.now().toEpochMilli
      val transaction = graph.transaction
      (1 to 10000).foreach { i =>
        val node = transaction.nodes.create()
        node --- `@id` --> s"some-iri-10,000-$i"
      }
      transaction
        .commit()

      val end      = java.time.Instant.now().toEpochMilli
      val duration = end - start
      println(s"create 10,000 nodes took ${duration} milli-seconds")
    }
    "create 20,000 nodes with an iri" ignore {
      val start       = java.time.Instant.now().toEpochMilli
      val transaction = graph.transaction
      (1 to 20000).foreach { i =>
        val node = transaction.nodes.create()
        node --- `@id` --> s"some-iri-20,000-$i"
      }
      transaction
        .commit()

      val end      = java.time.Instant.now().toEpochMilli
      val duration = end - start
      println(s"create 20,000 nodes took ${duration} milli-seconds")
    }
    "create 2x parallel 10,000 nodes with an iri" ignore {
      val start        = java.time.Instant.now().toEpochMilli
      val transaction1 = graph.transaction
      val transaction2 = graph.transaction
      Await.result(
        Future.sequence(
          Seq(
            Future {
              (1 to 10000).foreach { i =>
                val node = transaction1.nodes.create()
                node --- `@id` --> s"some-iri-10,000-1-$i"
              }
              transaction1
                .commit()
            },
            Future {
              (1 to 10000).foreach { i =>
                val node = transaction1.nodes.create()
                node --- `@id` --> s"some-iri-10,000-2-$i"
              }
              transaction2
                .commit()
            }
          )),
        300 seconds
      )
      val end      = java.time.Instant.now().toEpochMilli
      val duration = end - start
      println(s"create 2x 10,000 nodes in parallel took ${duration} milli-seconds")
    }
  }
}
