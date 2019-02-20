package lspace.librarian.logic.predicate

import lspace.librarian.logic.predicate._
import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Between
    extends PredicateDef("Between", `@extends` = () => List(RangeP.ontology))
    with PredicateWrapper[Between[_]] {

  def toP(node: Node): Between[_] = {
    val lower = node.out(RangeP.keys.lower).head
    val upper = node.out(RangeP.keys.upper).head
    Between(lower, upper)
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

case class Between[+T](lower: T, upper: T) extends RangeP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"between($lower, $upper)"
}
