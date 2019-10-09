package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object Lte extends PredicateDef("Lte", `@extends` = List(OrderP.ontology)) with PredicateWrapper[Lte[_]] {

  def toP(node: Node): Lte[_] = {
    val pvalue = node.out(EqP.keys.value).head
    Lte(pvalue)
  }

  object keys extends OrderP.Properties
  override lazy val properties: List[Property] = OrderP.properties
  trait Properties extends OrderP.Properties

  implicit def toNode[T](p: Lte[T]): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.value, ClassType.detect(p.pvalue), p.pvalue)
    } yield node
  }
}

case class Lte[+T](pvalue: T) extends OrderP[T] {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"lte($pvalue)"
}
