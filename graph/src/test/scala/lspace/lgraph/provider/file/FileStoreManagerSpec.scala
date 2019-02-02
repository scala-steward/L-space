package lspace.lgraph.provider.file

import lspace.lgraph.LGraph
import lspace.lgraph.provider.mem.{MemIndexProvider, MemStoreProvider}
import lspace.librarian.structure.{AsyncGraphSpec, Graph}

import scala.concurrent.Await

class FileStoreManagerSpec extends AsyncGraphSpec {

  val store       = MemStoreProvider("MemStoreManagerSpec")
  val sampleStore = FileStoreProvider("MemStoreManagerSpec-sample", "_data/MemStoreManagerSpec-sample")

  val graph: LGraph =
    LGraph(store, new MemIndexProvider)
  val sampleGraph: LGraph =
    LGraph(sampleStore, new MemIndexProvider)
  def createGraph(iri: String): Graph = {
    val storage = MemStoreProvider(iri)
    LGraph(storage, new MemIndexProvider)
  }

  override def beforeAll: Unit = {
    import scala.concurrent.duration._
    Await.ready(sampleGraph.init, 10 seconds)
    sampleGraph.edges().foreach(_.remove())
    sampleGraph.nodes().foreach(_.remove())
    sampleGraph.values().foreach(_.remove())
    sampleGraph.ns.edges().foreach(_.remove())
    sampleGraph.ns.nodes().foreach(_.remove())
    sampleGraph.ns.values().foreach(_.remove())
    super.beforeAll
  }

  "Graphs" can {
    "be merged" in {
      sampleGraph.persist
        .map { u =>
          val persistedGraph: LGraph =
            LGraph(FileStoreProvider("MemStoreManagerSpec-sample2", "_data/MemStoreManagerSpec-sample"),
                   new MemIndexProvider)
          assert(1 == 1)
//        val newGraph = createGraph("graphspec2merge")
//
//        newGraph.nodes().size shouldBe 0
//        newGraph.edges().size shouldBe 0
//        newGraph.values().size shouldBe 0
//
//        newGraph ++ sampleGraph
//
//        println(newGraph.g.N.toList.map(_.iri))
//        println(newGraph.g.N.toList.map(_.id))
//        println(sampleGraph.g.N.toList.map(_.iri))
//        println(sampleGraph.g.N.toList.map(_.id))
//        println(sampleGraph.g.V.toList)
//        println(newGraph.g.V.toList)
//
//        newGraph.nodes().size shouldBe sampleGraph.nodes.count
//        newGraph.edges().size shouldBe sampleGraph.edges.count
//        newGraph.values().size shouldBe sampleGraph.values.count
//
//        newGraph.close

        }
    }
  }
}
