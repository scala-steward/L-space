package lspace.lgraph.provider.mem

import lspace.lgraph.LGraph
import lspace.librarian.task.{Guide, StandardGuide, TaskSpec}
import lspace.structure.{Graph, GraphSpec, NodeSpec, SampledGraph}

class MemStoreManagerSpec extends GraphSpec with NodeSpec with TaskSpec {
  import lspace.Implicits.DefaultAssistent.assistent
  lazy val guide: Guide = StandardGuide()

  def createGraph(iri: String): Graph = {
    val storage = MemStoreProvider(iri)
    LGraph(storage, new MemIndexProvider)
  }

  lazy val graph: Graph = createGraph("MemStoreManagerSpec")
  lazy val sampleGraph  = SampledGraph(createGraph("MemStoreManagerSpec-sample"))
  sampleGraph.load

  graphTests(graph)
  sampledGraphTests(sampleGraph)
  nodeTests(graph)
  sampledNodeTests(sampleGraph)
  sampledGraphComputerTests(sampleGraph)
}
