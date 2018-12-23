package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Disjoint
    extends PredicateDef("Disjoint", `@extends` = () => List(ObjectP.ontology))
    with PredicateWrapper[Disjoint[_]] {

  def wrap(node: Node): Disjoint[_] = node match {
    case node: Disjoint[_] => node
    case _ =>
      val (pvalue, helper) = ObjectHelper map node.out(EqP.keys.value).head
      new Disjoint(pvalue, node)(helper)
  }

  object keys extends ObjectP.Properties
  override lazy val properties: List[Property] = ObjectP.properties
  trait Properties extends ObjectP.Properties

  def apply[T: ObjectHelper, T0, TT0 <: ClassType[_]](pvalue: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Disjoint[T] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(EqP.keys.value, pvalue)
    new Disjoint(pvalue, node)
  }
}

class Disjoint[T] private (val pvalue: T, override val value: Node)(implicit helper: ObjectHelper[T])
    extends WrappedNode(value)
    with ObjectP[T] {
  def assert(avalue: Any): Boolean = helper.disjoint(avalue, pvalue)

  override def prettyPrint: String = s"disjoint($pvalue)"
}
