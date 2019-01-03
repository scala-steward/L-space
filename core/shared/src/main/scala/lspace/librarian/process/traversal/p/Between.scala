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

  def toP(node: Node): Between[_] = {
    val (lower, helperLower) = RangeHelper map node.out(RangeP.keys.lower).head
    val (upper, helperUpper) = RangeHelper map node.out(RangeP.keys.upper).head
    Between(lower, upper)(helperLower)
  }

  object keys extends RangeP.Properties
  override lazy val properties: List[Property] = RangeP.properties
  trait Properties extends RangeP.Properties

  implicit def toNode[T](between: Between[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(RangeP.keys.lower, ClassType.valueToOntologyResource(between.lower), between.lower)
    node.addOut(RangeP.keys.upper, ClassType.valueToOntologyResource(between.upper), between.upper)
    node
  }
}

case class Between[T](lower: T, upper: T)(implicit helper: RangeHelper[T]) extends RangeP[T] {
  def assert(avalue: Any): Boolean =
    helper.gte(avalue, lower) && helper.lte(avalue, upper) //|| helperLower.eqv(avalue, lower) || helperUpper.eqv(avalue, upper)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"between($lower, $upper)"
}
