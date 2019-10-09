package lspace.librarian.logic.predicate

import lspace.librarian.logic.predicate.P._
import lspace.librarian.logic.predicate._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object Inside extends PredicateDef("Inside", `@extends` = List(RangeP.ontology)) with PredicateWrapper[Inside[_]] {

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

  implicit def toNode[T](p: Inside[T]): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.lower, ClassType.detect(p.lower), p.lower)
      _    <- node.addOut(keys.upper, ClassType.detect(p.upper), p.upper)
    } yield node
  }
}

case class Inside[+T](lower: T, upper: T) extends RangeP[T] {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"inside($lower, $upper)"
}
