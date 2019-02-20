package lspace.librarian.logic.predicate

import lspace.librarian.logic.predicate.P._
import lspace.librarian.logic.predicate.{PredicateDef, PredicateWrapper, SeqP}
import lspace.provider.detached.DetachedGraph
import lspace.structure._

object ContainsPrefix
    extends PredicateDef("ContainsPrefix", `@extends` = () => List(SeqP.ontology))
    with PredicateWrapper[ContainsPrefix[_]] {

  def toP(node: Node): ContainsPrefix[_] = {
    val pvalue = node.out(SeqP.keys.value).head
    ContainsPrefix(pvalue)
  }

  object keys extends SeqP.Properties
  override lazy val properties: List[Property] = SeqP.properties
  trait Properties extends SeqP.Properties

  implicit def toNode[T](containsPrefix: ContainsPrefix[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(SeqP.keys.value, ClassType.valueToOntologyResource(containsPrefix.pvalue), containsPrefix.pvalue)
    node
  }
}

case class ContainsPrefix[+T](pvalue: T) extends SeqP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"containsPrefix($pvalue)"
}
