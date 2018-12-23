package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Gte extends PredicateDef("Gte", `@extends` = () => List(OrderP.ontology)) with PredicateWrapper[Gte[_]] {

  def wrap(node: Node): Gte[_] = node match {
    case node: Gte[_] => node
    case _ =>
      val (pvalue, helper) = OrderHelper map node.out(EqP.keys.value).head
      new Gte(pvalue, node)(helper)
  }

  object keys extends OrderP.Properties
  override lazy val properties: List[Property] = OrderP.properties
  trait Properties extends OrderP.Properties

  def apply[T: OrderHelper, T0, TT0 <: ClassType[_]](pvalue: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Gte[T] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(EqP.keys.value, pvalue)
    new Gte(pvalue, node)
  }
}

class Gte[T] private (val pvalue: T, override val value: Node)(implicit helper: OrderHelper[T])
    extends WrappedNode(value)
    with OrderP[T] {
  def assert(avalue: Any): Boolean = helper.gte(avalue, pvalue)

  override def prettyPrint: String = s"gte($pvalue)"
}
