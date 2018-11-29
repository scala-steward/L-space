package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Lt extends PredicateCompanion("Lt") with PredicateWrapper[Lt[_]] {
  ontologyNode --- Property.default.`@extends` --> EqP.ontology

  def wrap(node: Node): Lt[_] = node match {
    case node: Lt[_] => node
    case _ =>
      val (pvalue, helper) = OrderHelper map node.out(EqP.keys.value).head
      new Lt(pvalue, node)(helper)
  }

  def apply[T: OrderHelper, T0, TT0 <: ClassType[_]](pvalue: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Lt[T] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(EqP.keys.value, pvalue)
    new Lt(pvalue, node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

class Lt[T] private (val pvalue: T, override val value: Node)(implicit helper: OrderHelper[T])
    extends WrappedNode(value)
    with OrderP[T] {
  def assert(avalue: Any): Boolean = helper.lt(avalue, pvalue)

  override def prettyPrint: String = s"lt($pvalue)"
}
