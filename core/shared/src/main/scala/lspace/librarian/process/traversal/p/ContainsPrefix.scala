package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal.{PredicateDef, PredicateWrapper, SeqP}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

object ContainsPrefix
    extends PredicateDef("ContainsPrefix", `@extends` = () => List(SeqP.ontology))
    with PredicateWrapper[ContainsPrefix[_]] {

  def toP(node: Node): ContainsPrefix[_] = {
    val (pvalue, helper) = StringHelper map node.out(SeqP.keys.value).head
    ContainsPrefix(pvalue)(helper)
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

case class ContainsPrefix[+T](pvalue: T)(implicit helper: StringHelper[T]) extends SeqP[T] {
  def assert(avalue: Any): Boolean = helper.containsPrefix(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"containsPrefix($pvalue)"
}
