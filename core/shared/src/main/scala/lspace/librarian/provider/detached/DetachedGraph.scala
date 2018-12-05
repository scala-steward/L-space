package lspace.librarian.provider.detached

import java.util.concurrent.atomic.AtomicLong

import lspace.librarian.process.computer.DefaultStreamComputer
import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.step._
import lspace.librarian.provider.mem._
import lspace.librarian.provider.mem.MemNode
import lspace.librarian.structure._
import lspace.librarian.structure.util.IdProvider

import scala.collection.mutable

object DetachedGraph extends MemDataGraph {
  lazy val iri: String = "detachedmemgraph"

  lazy val idProvider: IdProvider = new IdProvider {
    private val id = new AtomicLong()
    def next: Long = id.incrementAndGet
  }

  val ns: MemNSGraph = MemGraphDefault.ns
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

  override protected def _storeNode(node: _Node): Unit = {}

  override protected def _storeEdge(edge: _Edge[_, _]): Unit = {
    edge.from
      .asInstanceOf[MemResource[Any]]
      ._addOut(edge.asInstanceOf[Edge[Any, _]])
  }

  override protected def _indexEdge[S, E](edge: _Edge[S, E]): Unit = {}

  override protected def _storeValue(value: _Value[_]): Unit = {}

  override protected def _indexValue(value: _Value[_]): Unit = {}

//  override def nodes.upsert(uri: String, uris: Set[String] = Set()): Node = {
//    val node = createNode()
//    node.addOut(Property.default.typed.iriUrlString, uri)
//    //    node.property(MemGraphDefault.createdonDateTime, Instant.now())
//    node
//  }

  override val computer = new DefaultStreamComputer()
}
