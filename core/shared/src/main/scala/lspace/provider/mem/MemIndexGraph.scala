package lspace.provider.mem

import lspace.librarian.traversal.UntypedTraversal
import lspace.provider.mem.index.MemIndex
import lspace.structure.index.{Index, Indexes}
import lspace.structure.{IndexGraph, Property}
import monix.eval.Task

import scala.collection.mutable

trait MemIndexGraph extends MemGraph with IndexGraph {
  def graph: MemGraph

  object indexes extends Indexes(this) {
    protected val indexes: mutable.HashMap[UntypedTraversal, Index] =
      new mutable.HashMap[UntypedTraversal, Index]()

    override def `@idIndex`: Index = MemIndex(lspace.g.has(Property.default.`@id`).untyped)

    override def `@typeIndex`: Index = MemIndex(lspace.g.has(Property.default.`@type`).untyped)

    override def get(traversal: UntypedTraversal): Task[Option[Index]] = Task(indexes.get(traversal))

    override def create(traversal: UntypedTraversal): Task[Index] = {
      //    graph.nodes.upsert(traversal.toNode)
      Task(MemIndex(traversal))
    }

    override def delete(index: Index): Task[Unit] = Task {
      indexes.remove(index.traversal)
    }.void
  }

//  protected val `@patternIndex`: Index = createIndex(Vector(Set()))

//  override def _storeEdge[S, E](edge: _Edge[S, E]): Unit = super._storeEdge(edge)
}
