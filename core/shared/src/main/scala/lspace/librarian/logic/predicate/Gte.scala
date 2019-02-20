package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Gte extends PredicateDef("Gte", `@extends` = () => List(OrderP.ontology)) with PredicateWrapper[Gte[_]] {

  def toP(node: Node): Gte[_] = {
    val pvalue = node.out(EqP.keys.value).head
    Gte(pvalue)
  }

  object keys extends OrderP.Properties
  override lazy val properties: List[Property] = OrderP.properties
  trait Properties extends OrderP.Properties

  implicit def toNode[T](gte: Gte[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(OrderP.keys.value, ClassType.valueToOntologyResource(gte.pvalue), gte.pvalue)
    node
  }
}

case class Gte[+T](pvalue: T) extends OrderP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"gte($pvalue)"
}
