package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Gte extends PredicateDef("Gte", `@extends` = () => List(OrderP.ontology)) with PredicateWrapper[Gte[_]] {

  def toP(node: Node): Gte[_] = {
    val (pvalue, helper) = OrderHelper map node.out(EqP.keys.value).head
    Gte(pvalue)(helper)
  }

  object keys extends OrderP.Properties
  override lazy val properties: List[Property] = OrderP.properties
  trait Properties extends OrderP.Properties

  implicit def toNode[T](gte: Gte[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(OrderP.keys.value, ClassType.valueToOntologyResource(gte.pvalue), gte.pvalue)
    node
  }
}

case class Gte[T](pvalue: T)(implicit helper: OrderHelper[T]) extends OrderP[T] {
  def assert(avalue: Any): Boolean = helper.gte(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"gte($pvalue)"
}
