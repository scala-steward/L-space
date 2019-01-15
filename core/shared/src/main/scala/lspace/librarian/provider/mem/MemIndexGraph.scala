package lspace.librarian.provider.mem

import lspace.librarian.process.traversal.UntypedTraversal
import lspace.librarian.provider.mem.index.MemIndex
import lspace.librarian.structure.index.Index
import lspace.librarian.structure.{IndexGraph, Property}

import scala.collection.mutable

trait MemIndexGraph extends MemGraph with IndexGraph {
  def graph: MemGraph

  protected val indexes: mutable.HashMap[UntypedTraversal, Index] =
    new mutable.HashMap[UntypedTraversal, Index]()

  protected[mem] lazy val `@typeIndex`: Index = MemIndex(__[Any, Any].has(Property.default.`@type`).untyped)
//  protected val `@patternIndex`: Index = createIndex(Vector(Set()))

  def getIndex(traversal: UntypedTraversal): Option[Index] = indexes.get(traversal)
  protected def createIndex(traversal: UntypedTraversal): Index = {
//    graph.nodes.upsert(traversal.toNode)
    MemIndex(traversal)
  }

  def deleteIndex(index: Index): Unit = {
    indexes.remove(index.traversal)
  }
//  override def _storeEdge[S, E](edge: _Edge[S, E]): Unit = super._storeEdge(edge)
}
