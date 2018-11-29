package lspace.librarian.provider.mem

import java.util.concurrent.atomic.AtomicLong

import lspace.librarian.structure.util.IdProvider

object MemGraphDefault extends MemDataGraph {
  val iri: String = "memgraphdefault"
  println("create MemGraphDefault")
  private val _iri      = iri
  private lazy val self = this

  lazy val idProvider: IdProvider = new IdProvider {
    private val id = new AtomicLong()
    def next: Long = id.incrementAndGet
  }

  val ns: MemNSGraph = new MemNSGraph {
    def iri: String          = "memgraphdefault" + ".ns"
    private val _iri         = iri
    lazy val graph: MemGraph = self
    private val _thisgraph   = thisgraph
    lazy val index: MemIndexGraph = new MemIndexGraph {
      def iri: String = _iri + ".ns" + ".index"

      lazy val graph: MemGraph      = _thisgraph
      lazy val index: MemIndexGraph = this
    }
  }

  val index: MemIndexGraph = new MemIndexGraph {
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

  println("created MemGraphDefault")
}
