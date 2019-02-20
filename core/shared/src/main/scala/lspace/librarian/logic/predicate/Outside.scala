package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Outside
    extends PredicateDef("Outside", `@extends` = () => RangeP.ontology :: Nil)
    with PredicateWrapper[Outside[_]] {

  def toP(node: Node): Outside[_] = node match {
    case node: Outside[_] => node
    case _ =>
      val lower = node.out(RangeP.keys.lower).head
      val upper = node.out(RangeP.keys.upper).head
      Outside(lower, upper)
  }

  object keys extends RangeP.Properties
  override lazy val properties: List[Property] = RangeP.properties
  trait Properties extends RangeP.Properties

  implicit def toNode[T](outside: Outside[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(RangeP.keys.lower, ClassType.valueToOntologyResource(outside.lower), outside.lower)
    node.addOut(RangeP.keys.upper, ClassType.valueToOntologyResource(outside.upper), outside.upper)
    node
  }
}

case class Outside[+T](lower: T, upper: T) extends RangeP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"outside($lower, $upper)"
}
