package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P.{EqHelper, StringHelper}
import lspace.librarian.process.traversal.{EqP, P, PredicateDef, PredicateWrapper}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Regex extends PredicateDef("Regex", `@extends` = () => List(EqP.ontology)) with PredicateWrapper[Regex] {

  def wrap(node: Node): Regex = node match {
    case node: Regex => node
    case _           => new Regex(node.out(EqP.keys.value + DataType.default.`@string`).head.r, node)
  }

  object keys extends EqP.Properties
  override lazy val properties: List[Property] = EqP.properties
  trait Properties extends EqP.Properties

  def apply(pvalue: scala.util.matching.Regex): Regex = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(EqP.keys.value, pvalue.regex)
    new Regex(pvalue, node)
  }
}

class Regex private (val pvalue: scala.util.matching.Regex, override val value: Node)(
    implicit helper: StringHelper[String])
    extends WrappedNode(value)
    with EqP[String] {
  def assert(avalue: Any): Boolean = helper.regex(avalue, pvalue)

  override def prettyPrint: String = s"regex($pvalue)"
}
