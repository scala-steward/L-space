package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Intersect
    extends PredicateDef("Intersect", `@extends` = () => List(ObjectP.ontology))
    with PredicateWrapper[Intersect[_]] {

  def wrap(node: Node): Intersect[_] = node match {
    case node: Intersect[_] => node
    case _ =>
      val (pvalue, helper) = ObjectHelper map node.out(EqP.keys.value).head
      new Intersect(pvalue, node)(helper)
  }

  object keys extends ObjectP.Properties
  override lazy val properties: List[Property] = ObjectP.properties
  trait Properties extends ObjectP.Properties

  def apply[T: ObjectHelper, T0, TT0 <: ClassType[_]](pvalue: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Intersect[T] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(EqP.keys.value, pvalue)
    new Intersect(pvalue, node)
  }
}

class Intersect[T] private (val pvalue: T, override val value: Node)(implicit helper: ObjectHelper[T])
    extends WrappedNode(value)
    with ObjectP[T] {
  def assert(avalue: Any): Boolean = helper.intersect(avalue, pvalue)

  override def prettyPrint: String = s"intersect($pvalue)"
}
