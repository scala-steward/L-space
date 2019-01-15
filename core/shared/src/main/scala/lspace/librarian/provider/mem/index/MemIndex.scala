package lspace.librarian.provider.mem.index

import lspace.librarian.process.traversal.step.{Has, HasLabel, Out}
import lspace.librarian.process.traversal.{P, Step, UntypedTraversal}
import lspace.librarian.structure.Property
import lspace.librarian.structure.index.Index
import lspace.librarian.structure.index.shape.Shape

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

  val path: List[Out] = traversal.steps.collect { case step: Out => step }

  @tailrec
  private def splitByOut(patterns: List[Set[Property]], steps: List[Step]): List[Set[Property]] =
    steps match {
      case head :: tail =>
        tail.span(!_.isInstanceOf[Out]) match {
          case (l1, Nil) =>
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
      case Nil => patterns
    }

  val patterns: List[Set[Property]] =
    splitByOut(if (traversal.steps.head.isInstanceOf[Out]) List(Set()) else List(), traversal.steps)

  def store(shape: Shape): Unit = synchronized {
//    if (shape.edges.zipAll(path, null, null).forall {
//          case (null, p) => false
//          case (e, null) => false
//          case (e, p)    => p.label.contains(e.key)
//        }) {
    data += shape
//    }
  }

  def find(values: Vector[Map[Property, List[P[_]]]]): List[Shape] = {
    data.toStream.filter { shape =>
      (shape.origin :: shape.edges.map(_.to).toList).zipAll(values, null, null).forall {
        case (null, mpp) => false
        case (e, null)   => false
        case (e, List()) => true
        case (e, mpp)    => mpp.forall(mpp => e.out(mpp._1).exists(v => mpp._2.forall(p => p.assert(v))))
      }
    }.toList
  }

  def delete(shape: Shape): Unit = data -= shape
}
