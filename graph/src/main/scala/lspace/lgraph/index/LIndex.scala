package lspace.lgraph.index

import lspace.librarian.logic.predicate.P
import lspace.librarian.traversal.UntypedTraversal
import lspace.structure.Property
import lspace.structure.index.Index
import lspace.structure.index.shape.Shape
import monix.eval.Task
import monix.reactive.Observable

object LIndex {
  def apply(traversal: UntypedTraversal): LIndex = new LIndex(traversal)
}

class LIndex(val traversal: UntypedTraversal) extends Index {

  def store(shape: Shape): Task[Unit] = Task { synchronized {} }

  def find(values: Vector[Map[Property, List[P[_]]]]): Observable[Shape] =
    Observable()

  def delete(shape: Shape): Task[Unit] = Task.unit
}
