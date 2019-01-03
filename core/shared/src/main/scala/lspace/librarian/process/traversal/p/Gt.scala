package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Gt extends PredicateDef("Gt", `@extends` = () => List(OrderP.ontology)) with PredicateWrapper[Gt[_]] {

  def toP(node: Node): Gt[_] = {
    val (pvalue, helper) = OrderHelper map node.out(EqP.keys.value).head
    Gt(pvalue)(helper)
  }

  object keys extends OrderP.Properties
  override lazy val properties: List[Property] = OrderP.properties
  trait Properties extends OrderP.Properties

  implicit def toNode[T](gt: Gt[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(OrderP.keys.value, ClassType.valueToOntologyResource(gt.pvalue), gt.pvalue)
    node
  }
}

case class Gt[T](pvalue: T)(implicit helper: OrderHelper[T]) extends OrderP[T] {
  def assert(avalue: Any): Boolean = helper.gt(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"gt($pvalue)"
}
