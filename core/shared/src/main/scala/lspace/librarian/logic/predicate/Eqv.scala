package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object Eqv extends PredicateDef("Eqv", `@extends` = () => List(EqP.ontology)) with PredicateWrapper[Eqv[_]] {

  def toP(node: Node): Eqv[_] = {
    val pvalue = node.out(EqP.keys.value).head
    Eqv(pvalue)
  }

  object keys extends EqP.Properties
  override lazy val properties: List[Property] = EqP.properties
  trait Properties extends EqP.Properties

  implicit def toNode[T](p: Eqv[T]): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.value, ClassType.detect(p.pvalue), p.pvalue)
    } yield node
  }
}

case class Eqv[+T](pvalue: T) extends EqP[T] {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"eqv($pvalue)"
}
