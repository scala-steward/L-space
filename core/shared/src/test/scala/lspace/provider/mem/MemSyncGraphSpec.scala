package lspace.provider.mem

import lspace.librarian.task.SyncGuideSpec
import lspace.structure.{Graph, SampledGraph}

class MemSyncGraphSpec extends SyncGuideSpec {
  implicit lazy val guide = lspace.Implicits.SyncGuide.guide

  val graph: Graph = MemGraph("memgraphspec")
  val sampleGraph  = SampledGraph(MemGraph("memgraphspec-sample"))
  sampleGraph.load
  def createGraph(iri: String): Graph = MemGraph("memgraphspec-" + iri)

  sampledGraphComputerTests(sampleGraph)
}
