package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Lt extends PredicateDef("Lt", `@extends` = () => List(OrderP.ontology)) with PredicateWrapper[Lt[_]] {

  def toP(node: Node): Lt[_] = {
    val pvalue = node.out(EqP.keys.value).head
    Lt(pvalue)
  }

  object keys extends OrderP.Properties
  override lazy val properties: List[Property] = OrderP.properties
  trait Properties extends OrderP.Properties

  implicit def toNode[T](lt: Lt[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(OrderP.keys.value, ClassType.valueToOntologyResource(lt.pvalue), lt.pvalue)
    node
  }
}

case class Lt[+T](pvalue: T) extends OrderP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"lt($pvalue)"
}
