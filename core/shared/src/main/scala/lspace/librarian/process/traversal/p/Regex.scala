package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P.{EqHelper, StringHelper}
import lspace.librarian.process.traversal.{EqP, P, PredicateDef, PredicateWrapper}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Regex extends PredicateDef("Regex", `@extends` = () => List(EqP.ontology)) with PredicateWrapper[Regex] {

  def toP(node: Node): Regex = Regex(node.out(EqP.keys.value + DataType.default.`@string`).head.r)

  object keys extends EqP.Properties
  override lazy val properties: List[Property] = EqP.properties
  trait Properties extends EqP.Properties

  implicit def toNode(regex: Regex): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(EqP.keys.value, regex.pvalue.regex)
    node
  }
}

case class Regex(pvalue: scala.util.matching.Regex)(implicit helper: StringHelper[String]) extends EqP[String] {
  def assert(avalue: Any): Boolean = helper.regex(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"regex($pvalue)"
}
