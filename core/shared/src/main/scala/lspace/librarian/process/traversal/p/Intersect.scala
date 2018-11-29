package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Intersect extends PredicateCompanion("Intersect") with PredicateWrapper[Intersect[_]] {
  ontologyNode --- Property.default.`@extends` --> EqP.ontology

  def wrap(node: Node): Intersect[_] = node match {
    case node: Intersect[_] => node
    case _ =>
      val (pvalue, helper) = ObjectHelper map node.out(EqP.keys.value).head
      new Intersect(pvalue, node)(helper)
  }

  def apply[T: ObjectHelper, T0, TT0 <: ClassType[_]](pvalue: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Intersect[T] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(EqP.keys.value, pvalue)
    new Intersect(pvalue, node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

class Intersect[T] private (val pvalue: T, override val value: Node)(implicit helper: ObjectHelper[T])
    extends WrappedNode(value)
    with ObjectP[T] {
  def assert(avalue: Any): Boolean = helper.intersect(avalue, pvalue)

  override def prettyPrint: String = s"intersect($pvalue)"
}
