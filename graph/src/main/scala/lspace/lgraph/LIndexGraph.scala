package lspace.lgraph

import lspace.lgraph.index.{IndexManager, LIndex}
import lspace.librarian.traversal.UntypedTraversal
import lspace.provider.mem.MemGraph
import lspace.structure.index.Index
import lspace.structure.{IndexGraph, Property}
import monix.eval.Task
import monix.execution.CancelableFuture

import scala.collection.mutable

trait LIndexGraph extends LGraph with IndexGraph {
  def graph: LGraph
  def indexManager: IndexManager[this.type]
  override def ns: LNSGraph = graph.ns

  lazy val indexCache = MemGraph(s"${graph.iri}-cache")
  protected val indexes: mutable.HashMap[Long, Index] =
    new mutable.HashMap[Long, Index]()

  protected def `@typeIndex`: Index = LIndex(__[Any, Any].has(Property.default.`@type`).untyped)
//  protected val `@patternIndex`: Index = createIndex(Vector(Set()))

  def getIndex(traversal: UntypedTraversal): Task[Option[Index]] = Task.now(None) //indexes.get(pattern)
  protected def createIndex(traversal: UntypedTraversal): Task[Index] = Task {
//    val indexNode = graph.nodes.create(Index.ontology)
//    val patterns = pattern.map { p =>
//      val node = graph.nodes.create(Traversal.Segment.ontology)
//      p.foreach { ct =>
//        node.addOut(Traversal.Segment.keys.stepNode)
//      }
//      node
//    }

    val index = LIndex(traversal)

    index
  }

  def deleteIndex(index: Index): Task[Unit] = Task {
//    indexes.remove(pattern)
  }
//  override def _storeEdge[S, E](edge: _Edge[S, E]): Unit = super._storeEdge(edge)

//  override def persist: CancelableFuture[Unit] = CancelableFuture.unit //storeManager.persist
}
