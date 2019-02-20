package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Gt extends PredicateDef("Gt", `@extends` = () => List(OrderP.ontology)) with PredicateWrapper[Gt[_]] {

  def toP(node: Node): Gt[_] = {
    val pvalue = node.out(EqP.keys.value).head
    Gt(pvalue)
  }

  object keys extends OrderP.Properties
  override lazy val properties: List[Property] = OrderP.properties
  trait Properties extends OrderP.Properties

  implicit def toNode[T](gt: Gt[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(OrderP.keys.value, ClassType.valueToOntologyResource(gt.pvalue), gt.pvalue)
    node
  }
}

case class Gt[+T](pvalue: T) extends OrderP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"gt($pvalue)"
}
