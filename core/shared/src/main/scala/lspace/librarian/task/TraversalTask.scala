package lspace.librarian.task

import lspace.librarian.traversal.Segment
import lspace.structure.Graph
import monix.eval.Task
import monix.reactive.Observable
import shapeless.HList

import scala.collection.immutable.ListSet

trait TraversalTask[Out] {
  def segments: List[Segment[HList]]
  def graph: Graph
}
object TraversalAsyncTask {
//  type Aux[F[_]] = TraversalSyncTask[F]
}
case class TraversalAsyncTask[Out](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Observable])
    extends TraversalTask[Out] {
  def head: Task[Out] =
    guide.buildTraversal[Out](segments)(graph).headL

  def headOption: Task[Option[Out]] =
    guide.buildTraversal[Out](segments)(graph).headOptionL

  def toList: Task[List[Out]] =
    guide.buildTraversal[Out](segments)(graph).toListL

//  def toListSet: Task[ListSet[Out]] =
//    guide
//      .buildTraversal[Out](segments)(graph)
//      .toListL
//      .map(_.to[ListSet])
//
//  def toSet: Task[Set[Out]] =
//    guide.buildTraversal[Out](segments)(graph).toListL.map(_.toSet)

  def toObservable: Observable[Out] =
    guide.buildTraversal[Out](segments)(graph)

//  def toVector: Task[Vector[Out]] =
//    guide.buildTraversal[Out](segments)(graph).toListL.map(_.toVector)
}

case class TraversalSyncTask[Out](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Stream])
    extends TraversalTask[Out] {

  def head: Out =
    guide.buildTraversal[Out](segments)(graph).head

  def headOption: Option[Out] =
    guide.buildTraversal[Out](segments)(graph).headOption

  def toList: List[Out] =
    guide.buildTraversal[Out](segments)(graph).toList

//  def toListSet: ListSet[Out] =
//    guide.buildTraversal[Out](segments)(graph).toList.to[ListSet]
//
//  def toSet: Set[Out] =
//    guide.buildTraversal[Out](segments)(graph).toSet

  def toStream: Stream[Out] =
    guide.buildTraversal[Out](segments)(graph)

//  def toVector: Vector[Out] =
//    guide.buildTraversal[Out](segments)(graph).toVector
}
