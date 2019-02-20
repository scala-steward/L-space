package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Lte extends PredicateDef("Lte", `@extends` = () => List(OrderP.ontology)) with PredicateWrapper[Lte[_]] {

  def toP(node: Node): Lte[_] = {
    val pvalue = node.out(EqP.keys.value).head
    Lte(pvalue)
  }

  object keys extends OrderP.Properties
  override lazy val properties: List[Property] = OrderP.properties
  trait Properties extends OrderP.Properties

  implicit def toNode[T](lte: Lte[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(OrderP.keys.value, ClassType.valueToOntologyResource(lte.pvalue), lte.pvalue)
    node
  }
}

case class Lte[+T](pvalue: T) extends OrderP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"lte($pvalue)"
}
