package lspace.provider.detached

import lspace.provider.mem._
import lspace.structure._
import lspace.structure.util.IdProvider
import monix.eval.Task
import monix.execution.atomic.Atomic

object DetachedGraph extends MemDataGraph {
  lazy val iri: String = "detachedmemgraph"
  lazy val self        = this

  lazy val idProvider: IdProvider = new IdProvider {
    private val id       = Atomic(1000L)
    def next: Task[Long] = Task.now(id.incrementAndGet())
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

  override protected[lspace] def storeNode(node: _Node): Task[Unit] = Task.unit

  override protected[lspace] def storeEdge(edge: _Edge[_, _]): Task[Unit] = Task {
    edge.from
      .asInstanceOf[MemResource[Any]]
      ._addOut(edge.asInstanceOf[Edge[Any, _]])
  }

  override protected[lspace] def indexEdge[S, E](edge: _Edge[S, E]): Task[Unit] = Task.unit

  override protected[lspace] def storeValue(value: _Value[_]): Task[Unit] = Task.unit

//  override protected def _indexValue(value: _Value[_]): Unit = {}

//  override def nodes.upsert(uri: String, uris: Set[String] = Set()): Node = {
//    val node = createNode()
//    node.addOut(Property.default.typed.iriUrlString, uri)
//    //    node.property(MemGraphDefault.createdonDateTime, Instant.now())
//    node
//  }
}
