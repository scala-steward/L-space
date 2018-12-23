package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Lte extends PredicateDef("Lte", `@extends` = () => List(OrderP.ontology)) with PredicateWrapper[Lte[_]] {

  def wrap(node: Node): Lte[_] = node match {
    case node: Lte[_] => node
    case _ =>
      val (pvalue, helper) = OrderHelper map node.out(EqP.keys.value).head
      new Lte(pvalue, node)(helper)
  }

  object keys extends OrderP.Properties
  override lazy val properties: List[Property] = OrderP.properties
  trait Properties extends OrderP.Properties

  def apply[T: OrderHelper, T0, TT0 <: ClassType[_]](pvalue: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Lte[T] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(EqP.keys.value, pvalue)
    new Lte(pvalue, node)
  }
}

class Lte[T] private (val pvalue: T, override val value: Node)(implicit helper: OrderHelper[T])
    extends WrappedNode(value)
    with OrderP[T] {
  def assert(avalue: Any): Boolean = helper.lte(avalue, pvalue)

  override def prettyPrint: String = s"lte($pvalue)"
}
