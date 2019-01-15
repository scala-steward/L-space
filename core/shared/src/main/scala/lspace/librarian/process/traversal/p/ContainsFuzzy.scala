package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal.{PredicateDef, PredicateWrapper, SeqP}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

object ContainsFuzzy
    extends PredicateDef("ContainsFuzzy", `@extends` = () => List(SeqP.ontology))
    with PredicateWrapper[ContainsFuzzy[_]] {

  def toP(node: Node): ContainsFuzzy[_] = {
    val (pvalue, helper) = StringHelper map node.out(SeqP.keys.value).head
    ContainsFuzzy(pvalue)(helper)
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

case class ContainsFuzzy[+T](pvalue: T)(implicit helper: StringHelper[T]) extends SeqP[T] {
  def assert(avalue: Any): Boolean = helper.containsFuzzy(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"containsFuzzy($pvalue)"
}
