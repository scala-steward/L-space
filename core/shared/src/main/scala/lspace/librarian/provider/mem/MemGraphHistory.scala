package lspace.librarian.provider.mem

import lspace.librarian.structure.{DataGraph, History}

object MemGraphHistory {
  def apply(_iri: String): MemGraph = {
    MemGraphDefault.iri
    val graph = new MemGraphHistory {
      val iri: String  = _iri
      private val self = this
      val ns: MemNSGraph = new MemNSGraph {
        def iri: String     = _iri + ".ns"
        val graph: MemGraph = self
      }
      init()
    }

    graph
  }
}

trait MemGraphHistory extends MemGraph with DataGraph with History {
  def init(): Unit = {}
}
