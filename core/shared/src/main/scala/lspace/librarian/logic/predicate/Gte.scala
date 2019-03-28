package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object Gte extends PredicateDef("Gte", `@extends` = () => List(OrderP.ontology)) with PredicateWrapper[Gte[_]] {

  def toP(node: Node): Gte[_] = {
    val pvalue = node.out(EqP.keys.value).head
    Gte(pvalue)
  }

  object keys extends OrderP.Properties
  override lazy val properties: List[Property] = OrderP.properties
  trait Properties extends OrderP.Properties

  implicit def toNode[T](p: Gte[T]): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.value, ClassType.valueToOntologyResource(p.pvalue), p.pvalue)
    } yield node
  }
}

case class Gte[+T](pvalue: T) extends OrderP[T] {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"gte($pvalue)"
}
