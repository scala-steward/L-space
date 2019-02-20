package lspace.librarian.logic.predicate

import lspace.librarian.logic.predicate.P._
import lspace.librarian.logic.predicate._
import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Inside
    extends PredicateDef("Inside", `@extends` = () => List(RangeP.ontology))
    with PredicateWrapper[Inside[_]] {

  def toP(node: Node): Inside[_] = node match {
    case node: Inside[_] => node
    case _ =>
      val lower = node.out(RangeP.keys.lower).head
      val upper = node.out(RangeP.keys.upper).head
      Inside(lower, upper)
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

case class Inside[+T](lower: T, upper: T) extends RangeP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"inside($lower, $upper)"
}
