package lspace.structure.index

import lspace.librarian.traversal.UntypedTraversal
import lspace.structure.IndexGraph
import monix.eval.Task

abstract class Indexes(graph: IndexGraph) {

  def `@idIndex`: Index
  def `@typeIndex`: Index

  def get(traversal: UntypedTraversal): Task[Option[Index]]
  def create(traversal: UntypedTraversal): Task[Index]
  def getOrCreate(traversal: UntypedTraversal): Task[Index] = {
    //TODO: log when existing index is returned and no new index is created

    get(traversal).flatMap(_.map(Task.now).getOrElse(create(traversal)))
  }
  def delete(index: Index): Task[Unit]
}
