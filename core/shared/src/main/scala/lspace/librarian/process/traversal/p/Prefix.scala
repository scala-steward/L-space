package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.process.traversal.{EqP, PredicateDef, PredicateWrapper}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Prefix extends PredicateDef("Prefix", `@extends` = () => List(EqP.ontology)) with PredicateWrapper[Prefix[_]] {

  def wrap(node: Node): Prefix[_] = node match {
    case node: Prefix[_] => node
    case _ =>
      val (pvalue, helper) = StringHelper map node.out(EqP.keys.value).head
      new Prefix(pvalue, node)(helper)
  }

  object keys extends EqP.Properties
  override lazy val properties: List[Property] = EqP.properties
  trait Properties extends EqP.Properties

  def apply[T: StringHelper, T0, TT0 <: ClassType[_]](pvalue: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Prefix[T] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(EqP.keys.value, pvalue)
    new Prefix(pvalue, node)
  }
}

class Prefix[T] private (val pvalue: T, override val value: Node)(implicit helper: StringHelper[T])
    extends WrappedNode(value)
    with EqP[T] {
  def assert(avalue: Any): Boolean = helper.prefix(avalue, pvalue)

  override def prettyPrint: String = s"prefix($pvalue)"
}
