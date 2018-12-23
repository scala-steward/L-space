package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P.RangeHelper
import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Between
    extends PredicateDef("Between", `@extends` = () => List(RangeP.ontology))
    with PredicateWrapper[Between[_]] {

  def wrap(node: Node): Between[_] = node match {
    case node: Between[_] => node
    case _ =>
      val (lower, helperLower) = RangeHelper map node.out(RangeP.keys.lower).head
      val (upper, helperUpper) = RangeHelper map node.out(RangeP.keys.upper).head
      new Between(lower, upper, node)(helperLower)
  }

  object keys extends RangeP.Properties
  override lazy val properties: List[Property] = RangeP.properties
  trait Properties extends RangeP.Properties

  def apply[T: RangeHelper, T0, TT0 <: ClassType[_]](lower: T, upper: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Between[T] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(RangeP.keys.lower, lower)
    node.addOut(RangeP.keys.upper, upper)
    //    node.property(P.keys.dataTypeNode, dataType)
    new Between(lower, upper, node)
  }
}

case class Between[T] private (lower: T, upper: T, override val value: Node)(implicit helper: RangeHelper[T])
    extends WrappedNode(value)
    with RangeP[T] {
  def assert(avalue: Any): Boolean =
    helper.gte(avalue, lower) && helper.lte(avalue, upper) //|| helperLower.eqv(avalue, lower) || helperUpper.eqv(avalue, upper)

  override def prettyPrint: String = s"between($lower, $upper)"
}
