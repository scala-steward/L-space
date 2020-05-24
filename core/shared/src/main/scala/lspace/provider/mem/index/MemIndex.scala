package lspace.provider.mem.index

import lspace.librarian.logic.predicate.P
import lspace.librarian.traversal.UntypedTraversal
import lspace.librarian.traversal.step.{Has, HasLabel, Out, Step}
import lspace.structure.Property
import lspace.structure.index.Index
import lspace.structure.index.shape.Shape
import monix.eval.Task
import monix.reactive.Observable

import scala.annotation.tailrec
import scala.collection.mutable

object MemIndex {
//  def apply(shapes: Vector[Shape])(graph: MemGraph): MemIndex = new MemIndex(shapes, graph)
  def apply(shape: UntypedTraversal): MemIndex =
    new MemIndex(shape)
}

class MemIndex(val traversal: UntypedTraversal) extends Index {
  private val data: mutable.LinkedHashSet[Shape] =
    mutable.LinkedHashSet[Shape]()

  val path: Vector[Out] = traversal.steps.collect { case step: Out => step }

  @tailrec
  private def splitByOut(patterns: List[Set[Property]], steps: Vector[Step]): List[Set[Property]] =
    steps match {
      case head +: tail =>
        tail.span(!_.isInstanceOf[Out]) match {
          case (l1, Vector()) =>
            patterns :+ l1.collect {
              case step: Has      => step.key
              case step: HasLabel => Property.default.`@type`
            }.toSet
          case (l1, l2) =>
            splitByOut(patterns :+ l1.collect {
              case step: Has      => step.key
              case step: HasLabel => Property.default.`@type`
            }.toSet, l2)
        }
      case Vector() => patterns
    }

  val patterns: List[Set[Property]] =
    splitByOut(if (traversal.steps.head.isInstanceOf[Out]) List(Set()) else List(), traversal.steps)

  def store(shape: Shape): Task[Unit] =
    Task.now(synchronized {
//    if (shape.edges.zipAll(path, null, null).forall {
//          case (null, p) => false
//          case (e, null) => false
//          case (e, p)    => p.label.contains(e.key)
//        }) {
      data += shape
//    }
    })

  def find(values: Vector[Map[Property, List[P[_]]]]): Observable[Shape] =
    Observable.fromIterable(data.toStream.filter { shape =>
      (shape.origin :: shape.edges.map(_.to).toList).zipAll(values, null, null).forall {
        case (null, mpp) if mpp != null => false
        case (e, null) if e != null     => false
        case (e, List())                => true
//        case (e, mpp)    => mpp.forall(mpp => e.out(mpp._1).exists(v => mpp._2.forall(p => p.assert(v))))
      }
    }.toList)

  def delete(shape: Shape): Task[Unit] = Task.now(data -= shape)
}
