package lspace.provider.mem

import lspace.librarian.task.GuideSpec
import lspace.structure._
import lspace.structure.Property.default._

class MemGraphSpec extends GraphSpec with NodeSpec with GuideSpec {

  implicit lazy val guide = lspace.Implicits.StandardGuide.guide

  val graph: Graph = MemGraph("memgraphspec")
  val sampleGraph  = SampledGraph(MemGraph("memgraphspec-sample"))
  sampleGraph.load
  def createGraph(iri: String): Graph = MemGraph("memgraphspec-" + iri)

  graphTests(graph)
  sampledGraphTests(sampleGraph)
  nodeTests(graph)
  sampledNodeTests(sampleGraph)
  sampledGraphComputerTests(sampleGraph)

}
