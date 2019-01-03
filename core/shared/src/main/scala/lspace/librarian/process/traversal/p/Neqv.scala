package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P.EqHelper
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.process.traversal.{EqP, P, PredicateDef, PredicateWrapper}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Neqv extends PredicateDef("Neqv", `@extends` = () => List(EqP.ontology)) with PredicateWrapper[Neqv[_]] {

  def toP(node: Node): Neqv[_] = {
    val (pvalue, helper) = EqHelper map node.out(EqP.keys.value).head
    Neqv(pvalue)(helper)
  }

  object keys extends EqP.Properties
  override lazy val properties: List[Property] = EqP.properties
  trait Properties extends EqP.Properties

  implicit def toNode[T](neqv: Neqv[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(EqP.keys.value, ClassType.valueToOntologyResource(neqv.pvalue), neqv.pvalue)
    node
  }
}

case class Neqv[T](pvalue: T)(implicit helper: EqHelper[T]) extends EqP[T] {
  def assert(avalue: Any): Boolean = helper.neqv(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"neqv($pvalue)"
}
