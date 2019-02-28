package lspace.provider.mem

import lspace.librarian.task.AsyncGuideSpec
import lspace.structure._
import lspace.structure.Property.default._

class MemAsyncGraphSpec extends GraphSpec with NodeSpec with AsyncGuideSpec with NameSpaceGraphSpec {

  implicit lazy val guide = lspace.Implicits.AsyncGuide.guide

  val graph: Graph = MemGraph("memgraphspec")
  val sampleGraph  = SampledGraph(MemGraph("memgraphspec-sample"))
  sampleGraph.load
  def createGraph(iri: String): Graph = MemGraph("memgraphspec-" + iri)

  nameSpaceGraphTests(graph)
  graphTests(graph)
  sampledGraphTests(sampleGraph)
  nodeTests(graph)
  sampledNodeTests(sampleGraph)
  sampledGraphComputerTests(sampleGraph)

}
