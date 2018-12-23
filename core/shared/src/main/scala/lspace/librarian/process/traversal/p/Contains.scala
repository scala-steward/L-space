package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Contains
    extends PredicateDef("Contains", `@extends` = () => List(EqP.ontology))
    with PredicateWrapper[Contains[_]] {
  trait Helper[T] {
    def contains(avalue: Any, pvalue: T): Boolean
  }

  def wrap(node: Node): Contains[_] = node match {
    case node: Contains[_] => node
    case _ =>
      val (pvalue, helper) = node.out(EqP.keys.value).head match {
        case v: String => v -> Helper.TextHelper
        case _         => throw new Exception("No StringHelper found")
      }
      new Contains(pvalue, node)(helper)
  }

  object keys extends EqP.Properties
  override lazy val properties: List[Property] = EqP.properties
  trait Properties extends EqP.Properties

  def apply[T: EqHelper, T0, TT0 <: ClassType[_]](pvalue: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Contains[T] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(EqP.keys.value, pvalue)
    new Contains(pvalue, node)
  }
}

class Contains[T] private (val pvalue: T, override val value: Node)(implicit helper: EqHelper[T])
    extends WrappedNode(value)
    with EqP[T] {
  def assert(avalue: Any): Boolean = helper.contains(avalue, pvalue)

  override def prettyPrint: String = s"contains($pvalue)"
}
