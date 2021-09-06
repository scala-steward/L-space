package lspace.provider.mem

import lspace.structure.History
import lspace.structure.util.IdProvider
import monix.eval.Task
import monix.execution.atomic.Atomic

object MemGraphHistory {
  def apply(_iri: String): MemGraph = {
    val graph = new MemGraphHistory {
      val iri: String = _iri

      lazy val idProvider: IdProvider = new IdProvider {
        private val id       = Atomic(1000L)
        def next: Task[Long] = Task.now(id.incrementAndGet())
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
//          lazy val index: MemIndexGraph = this
        }
      }
      lazy val index: MemIndexGraph = new MemIndexGraph {
        def iri: String = _iri + ".index"

        lazy val graph: MemGraph = self
//        private val _thisgraph   = thisgraph
//        lazy val index: MemIndexGraph = new MemIndexGraph {
//          def iri: String = _iri + ".index" + ".index"
//
//          lazy val graph: MemGraph      = _thisgraph
//          lazy val index: MemIndexGraph = this
//        }
      }
      init
    }

    graph
  }
}

trait MemGraphHistory extends MemDataGraph with History {}
