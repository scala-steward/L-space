package lspace.provider.mem

import lspace.librarian.task.TaskSpec
import lspace.structure._
import lspace.structure.Property.default._

class MemGraphSpec extends GraphSpec with NodeSpec with TaskSpec {

  implicit lazy val guide = lspace.Implicits.StandardGuide.guide

  //  Ontology
  //  val graph: Graph = MemGraphDefault
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
