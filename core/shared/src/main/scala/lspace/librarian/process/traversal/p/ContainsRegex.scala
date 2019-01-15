package lspace.librarian.process.traversal.p

import lspace.librarian.datatype.DataType
import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal.{EqP, P, PredicateDef, PredicateWrapper, SeqP}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

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

case class ContainsRegex(pvalue: scala.util.matching.Regex)(implicit helper: StringHelper[String])
    extends SeqP[scala.util.matching.Regex] {
  def assert(avalue: Any): Boolean = helper.containsRegex(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"containsRegex($pvalue)"
}
