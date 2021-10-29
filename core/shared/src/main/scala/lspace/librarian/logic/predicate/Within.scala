package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object Within extends PredicateDef("Within", `@extends` = List(CollectionP.ontology)) with PredicateWrapper[Within[_]] {

  def toP(node: Node): Within[_] = Within(node.out(EqP.keys.value))

  object keys extends CollectionP.Properties
  override lazy val properties: List[Property] = CollectionP.properties
  trait Properties extends CollectionP.Properties

  implicit def toNode[T](p: Within[T]): Task[Node] =
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(EqP.keys.value, ClassType.detect(p.pvalue), p.pvalue)
    } yield node
}

case class Within[+T](pvalue: T) extends CollectionP[T] {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"within(${pvalue.toString})"
}
