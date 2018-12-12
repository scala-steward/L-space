package lspace.lgraph.provider.cassandra

import com.datastax.driver.core.SocketOptions
import com.outworkers.phantom.dsl.ContactPoint
import lspace.lgraph.LGraph
import lspace.lgraph.provider.elasticsearch.ESIndexProvider
import lspace.librarian.structure.{AsyncGraphSpec, Graph}

class AsyncCassandraStoreManagerSpec extends AsyncGraphSpec {
  val keySpaceBuilder =
    ContactPoint.local
      .withClusterBuilder(
        _.withSocketOptions(
          new SocketOptions()
            .setConnectTimeoutMillis(20000)
            .setReadTimeoutMillis(20000)
        ))
      .noHeartbeat()

  val store = LCassandraStoreProvider("AsyncCassandraStoreManagerSpec", keySpaceBuilder)
  store.deleteAll()
  val sampleStore = LCassandraStoreProvider("AsyncCassandraStoreManagerSpec-sample", keySpaceBuilder)
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
    super.afterAll()
  }

}
