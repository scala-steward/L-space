package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.process.traversal.{EqP, PredicateCompanion, PredicateWrapper}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Prefix extends PredicateCompanion("Prefix") with PredicateWrapper[Prefix[_]] {
  ontologyNode --- Property.default.`@extends` --> EqP.ontology

  def wrap(node: Node): Prefix[_] = node match {
    case node: Prefix[_] => node
    case _ =>
      val (pvalue, helper) = StringHelper map node.out(EqP.keys.value).head
      new Prefix(pvalue, node)(helper)
  }

  def apply[T: StringHelper, T0, TT0 <: ClassType[_]](pvalue: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Prefix[T] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(EqP.keys.value, pvalue)
    new Prefix(pvalue, node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

class Prefix[T] private (val pvalue: T, override val value: Node)(implicit helper: StringHelper[T])
    extends WrappedNode(value)
    with EqP[T] {
  def assert(avalue: Any): Boolean = helper.prefix(avalue, pvalue)

  override def prettyPrint: String = s"prefix($pvalue)"
}
