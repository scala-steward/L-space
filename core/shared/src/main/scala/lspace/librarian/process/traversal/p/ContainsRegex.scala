package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal.{EqP, P, PredicateDef, PredicateWrapper}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object ContainsRegex
    extends PredicateDef("ContainsRegex", `@extends` = () => List(EqP.ontology))
    with PredicateWrapper[ContainsRegex] {

  def wrap(node: Node): ContainsRegex = node match {
    case node: ContainsRegex => node
    case _                   => new ContainsRegex(node.out(EqP.keys.value + DataType.default.`@string`).head.r, node)
  }

  object keys extends EqP.Properties
  override lazy val properties: List[Property] = EqP.properties
  trait Properties extends EqP.Properties

  def apply(pvalue: scala.util.matching.Regex): ContainsRegex = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(EqP.keys.value, pvalue.regex)
    new ContainsRegex(pvalue, node)
  }
}

class ContainsRegex private (val pvalue: scala.util.matching.Regex, override val value: Node)(
    implicit helper: StringHelper[String])
    extends WrappedNode(value)
    with EqP[String] {
  def assert(avalue: Any): Boolean = helper.containsRegex(avalue, pvalue)

  override def prettyPrint: String = s"containsRegex($pvalue)"
}
