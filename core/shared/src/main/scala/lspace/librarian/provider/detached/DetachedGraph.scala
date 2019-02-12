package lspace.librarian.provider.detached

import lspace.librarian.process.computer.DefaultStreamComputer
import lspace.librarian.provider.mem._
import lspace.librarian.structure._
import lspace.librarian.structure.util.IdProvider
import monix.execution.atomic.Atomic

object DetachedGraph extends MemDataGraph {
  lazy val iri: String = "detachedmemgraph"
  lazy val self        = this

  lazy val idProvider: IdProvider = new IdProvider {
    private val id = Atomic(1000l)
    def next: Long = id.incrementAndGet()
  }

  val ns: MemNSGraph = new MemNSGraph {
    def iri: String = "detachedmemgraph.ns"

    lazy val graph: MemGraph    = self
    private lazy val _thisgraph = thisgraph
    lazy val index: MemIndexGraph = new MemIndexGraph {
      def iri: String = "detachedmemgraph.ns" + ".index"

      lazy val graph: MemGraph      = _thisgraph
      lazy val index: MemIndexGraph = this
    }
  }
  val index: MemIndexGraph = {
    lazy val self = thisgraph
    new MemIndexGraph {
      override def graph: MemGraph = self

      /**
        * An empty uri means that there is no URI assigned.
        *
        * @return
        */
      override def iri: String = self.iri + "/index"
    }
  }

  override protected[provider] def storeNode(node: GNode): Unit = {}

  override protected def storeEdge(edge: GEdge[_, _]): Unit = {
    edge.from
      .asInstanceOf[MemResource[Any]]
      ._addOut(edge.asInstanceOf[Edge[Any, _]])
  }

  override protected def _indexEdge[S, E](edge: GEdge[S, E]): Unit = {}

  override protected def storeValue(value: GValue[_]): Unit = {}

//  override protected def _indexValue(value: GValue[_]): Unit = {}

//  override def nodes.upsert(uri: String, uris: Set[String] = Set()): Node = {
//    val node = createNode()
//    node.addOut(Property.default.typed.iriUrlString, uri)
//    //    node.property(MemGraphDefault.createdonDateTime, Instant.now())
//    node
//  }

  override val computer = new DefaultStreamComputer()
}
