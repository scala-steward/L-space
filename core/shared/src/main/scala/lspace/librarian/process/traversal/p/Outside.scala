package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P.{OrderHelper, RangeHelper}
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.process.traversal.{P, PredicateCompanion, PredicateWrapper, RangeP}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Outside extends PredicateCompanion("Outside") with PredicateWrapper[Outside[_]] {
  ontologyNode --- Property.default.`@extends` --> RangeP.ontology

  def wrap(node: Node): Outside[_] = node match {
    case node: Outside[_] => node
    case _ =>
      val (lower, helperLower) = OrderHelper map node.out(RangeP.keys.lower).head
      val (upper, helperUpper) = OrderHelper map node.out(RangeP.keys.upper).head
      new Outside(lower, upper, node)(helperLower)
  }

  def apply[T: RangeHelper, T0, TT0 <: ClassType[_]](lower: T, upper: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Outside[T] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(RangeP.keys.lower, lower)
    node.addOut(RangeP.keys.upper, upper)
    //    node.property(P.keys.dataTypeNode, dataType)
    new Outside(lower, upper, node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

class Outside[T] private (val lower: T, val upper: T, override val value: Node)(implicit helper: OrderHelper[T])
    extends WrappedNode(value)
    with RangeP[T] {
  def assert(avalue: Any): Boolean = helper.lt(avalue, lower) || helper.gt(avalue, upper)

  override def prettyPrint: String = s"outside($lower, $upper)"
}
