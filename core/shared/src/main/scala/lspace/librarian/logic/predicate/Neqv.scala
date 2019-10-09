package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object Neqv extends PredicateDef("Neqv", `@extends` = EqP.ontology :: Nil) with PredicateWrapper[Neqv[_]] {

  def toP(node: Node): Neqv[_] = {
    val pvalue = node.out(EqP.keys.value).head
    Neqv(pvalue)
  }

  object keys extends EqP.Properties
  override lazy val properties: List[Property] = EqP.properties
  trait Properties extends EqP.Properties

  implicit def toNode[T](p: Neqv[T]): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.value, ClassType.detect(p.pvalue), p.pvalue)
    } yield node
  }
}

case class Neqv[+T](pvalue: T) extends EqP[T] {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"neqv($pvalue)"
}
