package lspace.provider.mem

import lspace.librarian.traversal.UntypedTraversal
import lspace.provider.mem.index.MemIndex
import lspace.structure.index.Index
import lspace.structure.{IndexGraph, Property}
import monix.eval.Task

import scala.collection.mutable

trait MemIndexGraph extends MemGraph with IndexGraph {
  def graph: MemGraph

  protected val indexes: mutable.HashMap[UntypedTraversal, Index] =
    new mutable.HashMap[UntypedTraversal, Index]()

  protected[mem] lazy val `@typeIndex`: Index = MemIndex(__[Any, Any].has(Property.default.`@type`).untyped)
//  protected val `@patternIndex`: Index = createIndex(Vector(Set()))

  def getIndex(traversal: UntypedTraversal): Task[Option[Index]] = Task.now(indexes.get(traversal))
  protected def createIndex(traversal: UntypedTraversal): Task[Index] = {
//    graph.nodes.upsert(traversal.toNode)
    Task.now(MemIndex(traversal))
  }

  def deleteIndex(index: Index): Task[Unit] = Task.now {
    indexes.remove(index.traversal)
  }
//  override def _storeEdge[S, E](edge: _Edge[S, E]): Unit = super._storeEdge(edge)
}
