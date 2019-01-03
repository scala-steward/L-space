package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.process.traversal.{EqP, PredicateDef, PredicateWrapper}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Suffix extends PredicateDef("Suffix", `@extends` = () => List(EqP.ontology)) with PredicateWrapper[Suffix[_]] {

  def toP(node: Node): Suffix[_] = {

    val (pvalue, helper) = StringHelper map node.out(EqP.keys.value).head
    Suffix(pvalue)(helper)
  }

  object keys extends EqP.Properties
  override lazy val properties: List[Property] = EqP.properties
  trait Properties extends EqP.Properties

  implicit def toNode[T](suffix: Suffix[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(EqP.keys.value, ClassType.valueToOntologyResource(suffix.pvalue), suffix.pvalue)
    node
  }
}

case class Suffix[T](pvalue: T)(implicit helper: StringHelper[T]) extends EqP[T] {
  def assert(avalue: Any): Boolean = helper.suffix(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"suffix($pvalue)"
}
