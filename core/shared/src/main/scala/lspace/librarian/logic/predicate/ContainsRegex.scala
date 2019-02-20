package lspace.librarian.logic.predicate

import lspace.datatype.DataType
import lspace.librarian.logic.predicate.P._
import lspace.librarian.logic.predicate.{EqP, P, PredicateDef, PredicateWrapper, SeqP}
import lspace.provider.detached.DetachedGraph
import lspace.structure._

object ContainsRegex
    extends PredicateDef("ContainsRegex", `@extends` = () => List(SeqP.ontology))
    with PredicateWrapper[ContainsRegex] {

  def toP(node: Node): ContainsRegex = ContainsRegex(node.out(SeqP.keys.value + DataType.default.`@string`).head.r)

  object keys extends SeqP.Properties
  override lazy val properties: List[Property] = SeqP.properties
  trait Properties extends SeqP.Properties

  implicit def toNode[T](containsRegex: ContainsRegex): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(SeqP.keys.value, ClassType.valueToOntologyResource(containsRegex.pvalue), containsRegex.pvalue)
    node
  }
}

case class ContainsRegex(pvalue: scala.util.matching.Regex) extends SeqP[scala.util.matching.Regex] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"containsRegex($pvalue)"
}
