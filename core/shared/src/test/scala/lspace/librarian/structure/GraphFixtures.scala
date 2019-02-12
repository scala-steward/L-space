package lspace.librarian.structure

import lspace.librarian.util.SampleGraph

case class SampledGraph(graph: Graph) {
  lazy val load = SampleGraph.loadSocial(graph)
}

trait GraphFixtures {
  def createGraph(iri: String): Graph
  def createSampleGraph(iri: String): SampledGraph = SampledGraph(createGraph(iri))
}
