package lspace.librarian.provider.mem

import lspace.librarian.structure.{AsyncGraphSpec, Graph}

class AsyncMemGraphSpec extends AsyncGraphSpec {
  val graph: Graph                    = MemGraph("asyncmemgraphspec")
  val sampleGraph: Graph              = MemGraph("asyncmemgraphspec-sample")
  def createGraph(iri: String): Graph = MemGraph("asyncmemgraphspec-" + iri)

}
