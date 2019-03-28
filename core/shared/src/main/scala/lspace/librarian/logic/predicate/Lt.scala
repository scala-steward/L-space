package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object Lt extends PredicateDef("Lt", `@extends` = () => List(OrderP.ontology)) with PredicateWrapper[Lt[_]] {

  def toP(node: Node): Lt[_] = {
    val pvalue = node.out(EqP.keys.value).head
    Lt(pvalue)
  }

  object keys extends OrderP.Properties
  override lazy val properties: List[Property] = OrderP.properties
  trait Properties extends OrderP.Properties

  implicit def toNode[T](p: Lt[T]): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.value, ClassType.valueToOntologyResource(p.pvalue), p.pvalue)
    } yield node
  }
}

case class Lt[+T](pvalue: T) extends OrderP[T] {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"lt($pvalue)"
}
