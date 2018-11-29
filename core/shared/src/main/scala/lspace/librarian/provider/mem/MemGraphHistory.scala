package lspace.librarian.provider.mem

import java.util.concurrent.atomic.AtomicLong

import lspace.librarian.structure.util.IdProvider
import lspace.librarian.structure.{DataGraph, History}

object MemGraphHistory {
  def apply(_iri: String): MemGraph = {
    MemGraphDefault.iri
    val graph = new MemGraphHistory {
      val iri: String = _iri

      lazy val idProvider: IdProvider = new IdProvider {
        private val id = new AtomicLong()
        id.set(1000)
        def next: Long = id.incrementAndGet
      }

      private val self = this
      lazy val ns: MemNSGraph = new MemNSGraph {
        def iri: String          = _iri + ".ns"
        private val _iri         = iri
        lazy val graph: MemGraph = self
        private val _thisgraph   = thisgraph
        lazy val index: MemIndexGraph = new MemIndexGraph {
          def iri: String = _iri + ".ns" + ".index"

          lazy val graph: MemGraph      = _thisgraph
          lazy val index: MemIndexGraph = this
        }
      }
      lazy val index: MemIndexGraph = new MemIndexGraph {
        def iri: String = _iri + ".index"

        lazy val graph: MemGraph = self
        private val _thisgraph   = thisgraph
        lazy val index: MemIndexGraph = new MemIndexGraph {
          def iri: String = _iri + ".index" + ".index"

          lazy val graph: MemGraph      = _thisgraph
          lazy val index: MemIndexGraph = this
        }
      }
      init()
    }

    graph
  }
}

trait MemGraphHistory extends MemGraph with DataGraph with History {}
