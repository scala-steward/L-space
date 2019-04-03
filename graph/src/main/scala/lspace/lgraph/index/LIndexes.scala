package lspace.lgraph.index

import lspace.lgraph.LIndexGraph
import lspace.librarian.traversal.UntypedTraversal
import lspace.structure.{IndexGraph, Property}
import lspace.structure.index.{Index, Indexes}
import monix.eval.Task

import scala.collection.mutable

abstract class LIndexes(graph: LIndexGraph) extends Indexes(graph) {
  protected val indexes: mutable.HashMap[Long, Index] =
    new mutable.HashMap[Long, Index]()

  def `@idIndex`: Index = LIndex(lspace.g.has(Property.default.`@id`).untyped)

  def `@typeIndex`: Index = LIndex(lspace.g.has(Property.default.`@type`).untyped)

  def get(traversal: UntypedTraversal): Task[Option[Index]] = Task.now(None)

  def create(traversal: UntypedTraversal): Task[Index] = Task {
    val index = LIndex(traversal)
    index
  }

  def delete(index: Index): Task[Unit] = Task.unit
}
