package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P.EqHelper
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.process.traversal.{EqP, PredicateDef, PredicateWrapper}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Eqv extends PredicateDef("Eqv", `@extends` = () => List(EqP.ontology)) with PredicateWrapper[Eqv[_]] {

  def toP(node: Node): Eqv[_] = {
    val (pvalue, helper) = EqHelper map node.out(EqP.keys.value).head
    Eqv(pvalue)(helper)
  }

  object keys extends EqP.Properties
  override lazy val properties: List[Property] = EqP.properties
  trait Properties extends EqP.Properties

  implicit def toNode[T](eqv: Eqv[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(EqP.keys.value, ClassType.valueToOntologyResource(eqv.pvalue), eqv.pvalue)
    node
  }
}

case class Eqv[+T](pvalue: T)(implicit helper: EqHelper[T]) extends EqP[T] {
  def assert(avalue: Any): Boolean = helper.eqv(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"eqv($pvalue)"
}
