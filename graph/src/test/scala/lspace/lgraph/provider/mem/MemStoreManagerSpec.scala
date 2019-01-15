package lspace.lgraph.provider.mem

import lspace.lgraph.LGraph
import lspace.lgraph.provider.elasticsearch.ESIndexProvider
import lspace.librarian.process.computer.GraphComputerSpec
import lspace.librarian.structure.{Graph, GraphSpec, NodeSpec}

class MemStoreManagerSpec extends GraphSpec with NodeSpec with GraphComputerSpec {

  val store       = MemStoreProvider("MemStoreManagerSpec")
  val sampleStore = MemStoreProvider("MemStoreManagerSpec-sample")

  val graph: LGraph =
    LGraph(store, new ESIndexProvider)
  val sampleGraph: LGraph =
    LGraph(sampleStore, new ESIndexProvider)
  def createGraph(iri: String): Graph = {
    val storage = MemStoreProvider(iri)
    LGraph(storage, new ESIndexProvider)
  }

  override def beforeAll: Unit = {
    super.beforeAll
  }

  override def afterAll(): Unit = {
    super.afterAll()
  }
}
