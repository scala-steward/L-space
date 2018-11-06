package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.process.traversal.{EqP, P, PredicateCompanion, PredicateWrapper}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Fuzzy extends PredicateCompanion("Fuzzy") with PredicateWrapper[Fuzzy[_]] {
  ontologyNode --- Property.default.EXTENDS --> EqP.ontology

  def wrap(node: Node): Fuzzy[_] = node match {
    case node: Fuzzy[_] => node
    case _ =>
      val (pvalue, helper) = StringHelper map node.out(EqP.keys.value).head
      new Fuzzy(pvalue, node)(helper)
  }

  def apply[T: StringHelper, T0, TT0 <: ClassType[_]](pvalue: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Fuzzy[T] = {
    val node = DetachedGraph.createNode(ontology)

    node.addOut(EqP.keys.value, pvalue)
    new Fuzzy(pvalue, node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

class Fuzzy[T] private (val pvalue: T, override val value: Node)(implicit helper: StringHelper[T])
    extends WrappedNode(value)
    with EqP[T] {
  def assert(avalue: Any): Boolean = helper.fuzzy(avalue, pvalue)

  override def prettyPrint: String = s"fuzzy($pvalue)"
}
