package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object Outside extends PredicateDef("Outside", `@extends` = RangeP.ontology :: Nil) with PredicateWrapper[Outside[_]] {

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

  implicit def toNode[T](p: Outside[T]): Task[Node] =
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.lower, ClassType.detect(p.lower), p.lower)
      _    <- node.addOut(keys.upper, ClassType.detect(p.upper), p.upper)
    } yield node
}

case class Outside[+T](lower: T, upper: T) extends RangeP[T] {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"outside($lower, $upper)"
}
