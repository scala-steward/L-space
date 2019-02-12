package lspace.librarian.provider.mem

import lspace.librarian.process.computer.GraphComputerSpec
import lspace.librarian.structure._
import lspace.librarian.structure.Property.default._

class MemGraphSpec extends GraphSpec with NodeSpec with GraphComputerSpec {

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
