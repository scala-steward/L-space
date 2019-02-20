package lspace.librarian.logic.predicate

import lspace.datatype.DataType
import lspace.provider.detached.DetachedGraph
import lspace.structure._

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

case class Regex(pvalue: scala.util.matching.Regex) extends SeqP[scala.util.matching.Regex] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"regex($pvalue)"
}
