package lspace.librarian.logic.predicate

import lspace.librarian.logic.predicate.P._
import lspace.librarian.logic.predicate.{PredicateDef, PredicateWrapper, SeqP}
import lspace.provider.detached.DetachedGraph
import lspace.structure._

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

  implicit def toNode[T](containsFuzzy: ContainsFuzzy[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(SeqP.keys.value, ClassType.valueToOntologyResource(containsFuzzy.pvalue), containsFuzzy.pvalue)
    node
  }
}

case class ContainsFuzzy[+T](pvalue: T) extends SeqP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"containsFuzzy($pvalue)"
}
