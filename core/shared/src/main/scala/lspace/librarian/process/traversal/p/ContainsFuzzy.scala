package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.process.traversal.{EqP, P, PredicateCompanion, PredicateWrapper}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object ContainsFuzzy extends PredicateCompanion("ContainsFuzzy") with PredicateWrapper[ContainsFuzzy[_]] {
  ontologyNode --- Property.default.EXTENDS --> EqP.ontology

  def wrap(node: Node): ContainsFuzzy[_] = node match {
    case node: ContainsFuzzy[_] => node
    case _ =>
      val (pvalue, helper) = StringHelper map node.out(EqP.keys.value).head
      new ContainsFuzzy(pvalue, node)(helper)
  }

  def apply[T: StringHelper, T0, TT0 <: ClassType[_]](pvalue: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): ContainsFuzzy[T] = {
    val node = DetachedGraph.createNode(ontology)

    node.addOut(EqP.keys.value, pvalue)
    new ContainsFuzzy(pvalue, node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

class ContainsFuzzy[T] private (val pvalue: T, override val value: Node)(implicit helper: StringHelper[T])
    extends WrappedNode(value)
    with EqP[T] {
  def assert(avalue: Any): Boolean = helper.containsFuzzy(avalue, pvalue)

  override def prettyPrint: String = s"containsFuzzy($pvalue)"
}
