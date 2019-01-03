package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Inside
    extends PredicateDef("Inside", `@extends` = () => List(RangeP.ontology))
    with PredicateWrapper[Inside[_]] {

  def toP(node: Node): Inside[_] = node match {
    case node: Inside[_] => node
    case _ =>
      val (lower, helperLower) = OrderHelper map node.out(RangeP.keys.lower).head
      val (upper, helperUpper) = OrderHelper map node.out(RangeP.keys.upper).head
      Inside(lower, upper)(helperLower)
  }

  object keys extends RangeP.Properties
  override lazy val properties: List[Property] = RangeP.properties
  trait Properties extends RangeP.Properties

  implicit def toNode[T](inside: Inside[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(RangeP.keys.lower, ClassType.valueToOntologyResource(inside.lower), inside.lower)
    node.addOut(RangeP.keys.upper, ClassType.valueToOntologyResource(inside.upper), inside.upper)
    node
  }
}

case class Inside[T](lower: T, upper: T)(implicit helper: OrderHelper[T]) extends RangeP[T] {
  def assert(avalue: Any): Boolean = helper.gt(avalue, lower) && helper.lt(avalue, upper)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"inside($lower, $upper)"
}
