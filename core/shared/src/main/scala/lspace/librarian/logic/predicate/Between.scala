package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

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

  implicit def toNode[T](p: Between[T]): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.lower, ClassType.valueToOntologyResource(p.lower), p.lower)
      _    <- node.addOut(keys.upper, ClassType.valueToOntologyResource(p.upper), p.upper)
    } yield node
  }
}

case class Between[+T](lower: T, upper: T) extends RangeP[T] {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"between($lower, $upper)"
}
