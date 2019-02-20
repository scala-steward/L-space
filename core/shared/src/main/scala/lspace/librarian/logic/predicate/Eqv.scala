package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Eqv extends PredicateDef("Eqv", `@extends` = () => List(EqP.ontology)) with PredicateWrapper[Eqv[_]] {

  def toP(node: Node): Eqv[_] = {
    val pvalue = node.out(EqP.keys.value).head
    Eqv(pvalue)
  }

  object keys extends EqP.Properties
  override lazy val properties: List[Property] = EqP.properties
  trait Properties extends EqP.Properties

  implicit def toNode[T](eqv: Eqv[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(EqP.keys.value, ClassType.valueToOntologyResource(eqv.pvalue), eqv.pvalue)
    node
  }
}

case class Eqv[+T](pvalue: T) extends EqP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"eqv($pvalue)"
}
