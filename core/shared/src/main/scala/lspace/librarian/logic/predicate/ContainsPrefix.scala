package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object ContainsPrefix
    extends PredicateDef("ContainsPrefix", `@extends` = List(SeqP.ontology))
    with PredicateWrapper[ContainsPrefix[_]] {

  def toP(node: Node): ContainsPrefix[_] = {
    val pvalue = node.out(SeqP.keys.value).head
    ContainsPrefix(pvalue)
  }

  object keys extends SeqP.Properties
  override lazy val properties: List[Property] = SeqP.properties
  trait Properties extends SeqP.Properties

  implicit def toNode[T](p: ContainsPrefix[T]): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.value, ClassType.detect(p.pvalue), p.pvalue)
    } yield node
  }
}

case class ContainsPrefix[+T](pvalue: T) extends SeqP[T] {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"containsPrefix($pvalue)"
}
