package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Neqv extends PredicateDef("Neqv", `@extends` = () => EqP.ontology :: Nil) with PredicateWrapper[Neqv[_]] {

  def toP(node: Node): Neqv[_] = {
    val pvalue = node.out(EqP.keys.value).head
    Neqv(pvalue)
  }

  object keys extends EqP.Properties
  override lazy val properties: List[Property] = EqP.properties
  trait Properties extends EqP.Properties

  implicit def toNode[T](neqv: Neqv[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(EqP.keys.value, ClassType.valueToOntologyResource(neqv.pvalue), neqv.pvalue)
    node
  }
}

case class Neqv[+T](pvalue: T) extends EqP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"neqv($pvalue)"
}
