package lspace.librarian.process.traversal.p

import lspace.librarian.datatype.DataType
import lspace.librarian.process.traversal.P.StringHelper
import lspace.librarian.process.traversal.{PredicateDef, PredicateWrapper, SeqP}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

object Regex extends PredicateDef("Regex", `@extends` = () => List(SeqP.ontology)) with PredicateWrapper[Regex] {

  def toP(node: Node): Regex = Regex(node.out(SeqP.keys.value + DataType.default.`@string`).head.r)

  object keys extends SeqP.Properties
  override lazy val properties: List[Property] = SeqP.properties
  trait Properties extends SeqP.Properties

  implicit def toNode(regex: Regex): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(SeqP.keys.value, regex.pvalue.regex)
    node
  }
}

case class Regex(pvalue: scala.util.matching.Regex)(implicit helper: StringHelper[String])
    extends SeqP[scala.util.matching.Regex] {
  def assert(avalue: Any): Boolean = helper.regex(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"regex($pvalue)"
}
