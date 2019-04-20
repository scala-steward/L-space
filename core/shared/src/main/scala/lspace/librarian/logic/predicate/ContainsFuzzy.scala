package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object ContainsFuzzy
    extends PredicateDef("ContainsFuzzy", `@extends` = () => List(SeqP.ontology))
    with PredicateWrapper[ContainsFuzzy[_]] {

  def toP(node: Node): ContainsFuzzy[_] = {
    val pvalue = node.out(SeqP.keys.value).head
    ContainsFuzzy(pvalue)
  }

  object keys extends SeqP.Properties
  override lazy val properties: List[Property] = SeqP.properties
  trait Properties extends SeqP.Properties

  implicit def toNode[T](p: ContainsFuzzy[T]): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.value, ClassType.valueToOntologyResource(p.pvalue), p.pvalue)
    } yield node
  }
}

case class ContainsFuzzy[+T](pvalue: T) extends SeqP[T] {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"containsFuzzy($pvalue)"
}
