package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.{EqP, ObjectP, PredicateCompanion, PredicateWrapper}
import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.types.vector.Geometry

object GeoContains extends PredicateCompanion("GeoContains") with PredicateWrapper[GeoContains[_]] {
  ontologyNode --- Property.default.`@extends` --> ObjectP.ontology

  def wrap(node: Node): GeoContains[_] = node match {
    case node: GeoContains[_] => node
    case _ =>
      val (pvalue, helper) = node.out(EqP.keys.value).head match {
        case v: Geometry => v -> Helper.GeoHelper
        case _           => throw new Exception("No StringHelper found")
      }
      new GeoContains(pvalue, node)(helper)
  }

  def apply[T: ObjectHelper, T0, TT0 <: ClassType[_]](pvalue: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): GeoContains[T] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(EqP.keys.value, pvalue)
    new GeoContains(pvalue, node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

class GeoContains[T] private (val pvalue: T, override val value: Node)(implicit helper: ObjectHelper[T])
    extends WrappedNode(value)
    with ObjectP[T] {
  def assert(avalue: Any): Boolean = helper.contains(avalue, pvalue)

  override def prettyPrint: String = s"contains($pvalue)"
}
