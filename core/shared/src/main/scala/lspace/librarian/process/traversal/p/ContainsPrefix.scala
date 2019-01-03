package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.process.traversal.{EqP, P, PredicateDef, PredicateWrapper}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object ContainsPrefix
    extends PredicateDef("ContainsPrefix", `@extends` = () => List(EqP.ontology))
    with PredicateWrapper[ContainsPrefix[_]] {

  def toP(node: Node): ContainsPrefix[_] = {
    val (pvalue, helper) = StringHelper map node.out(EqP.keys.value).head
    ContainsPrefix(pvalue)(helper)
  }

  object keys extends EqP.Properties
  override lazy val properties: List[Property] = EqP.properties
  trait Properties extends EqP.Properties

  implicit def toNode[T](containsPrefix: ContainsPrefix[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(EqP.keys.value, ClassType.valueToOntologyResource(containsPrefix.pvalue), containsPrefix.pvalue)
    node
  }
}

case class ContainsPrefix[T](pvalue: T)(implicit helper: StringHelper[T]) extends EqP[T] {
  def assert(avalue: Any): Boolean = helper.containsPrefix(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"containsPrefix($pvalue)"
}
