package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.process.traversal.{EqP, PredicateCompanion, PredicateWrapper}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Suffix extends PredicateCompanion("Suffix") with PredicateWrapper[Suffix[_]] {
  ontologyNode --- Property.default.EXTENDS --> EqP.ontology

  def wrap(node: Node): Suffix[_] = node match {
    case node: Suffix[_] => node
    case _ =>
      val (pvalue, helper) = StringHelper map node.out(EqP.keys.value).head
      new Suffix(pvalue, node)(helper)
  }

  def apply[T: StringHelper, T0, TT0 <: ClassType[_]](pvalue: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Suffix[T] = {
    val node = DetachedGraph.createNode(ontology)

    node.addOut(EqP.keys.value, pvalue)
    new Suffix(pvalue, node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

class Suffix[T] private (val pvalue: T, override val value: Node)(implicit helper: StringHelper[T])
    extends WrappedNode(value)
    with EqP[T] {
  def assert(avalue: Any): Boolean = helper.suffix(avalue, pvalue)

  override def prettyPrint: String = s"suffix($pvalue)"
}
