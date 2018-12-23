package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P.{Helper, ObjectHelper}
import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.types.vector.Geometry

object GeoWithin
    extends PredicateDef("GeoWithin", `@extends` = () => List(ObjectP.ontology))
    with PredicateWrapper[GeoWithin[_]] {

  def wrap(node: Node): GeoWithin[_] = node match {
    case node: GeoWithin[_] => node
    case _ =>
      val (pvalue, helper) = node.out(EqP.keys.value).head match {
        case v: Geometry => v -> Helper.GeoHelper
        case _           => throw new Exception("No StringHelper found")
      }
      new GeoWithin(pvalue, node)(helper)
  }

  object keys extends ObjectP.Properties
  override lazy val properties: List[Property] = ObjectP.properties
  trait Properties extends ObjectP.Properties

  def apply[T: ObjectHelper, T0, TT0 <: ClassType[_]](pvalue: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): GeoWithin[T] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(EqP.keys.value, pvalue)
    new GeoWithin(pvalue, node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

class GeoWithin[T] private (val pvalue: T, override val value: Node)(implicit helper: ObjectHelper[T])
    extends WrappedNode(value)
    with ObjectP[T] {
  def assert(avalue: Any): Boolean = helper.within(avalue, pvalue)

  override def prettyPrint: String = s"within($pvalue)"
}
