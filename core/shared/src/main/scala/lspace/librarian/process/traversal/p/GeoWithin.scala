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

  def toP(node: Node): GeoWithin[_] = {
    val (pvalue, helper) = node.out(ObjectP.keys.value).head match {
      case v: Geometry => v -> Helper.GeoHelper
      case _           => throw new Exception("No StringHelper found")
    }
    GeoWithin(pvalue)(helper)
  }

  object keys extends ObjectP.Properties
  override lazy val properties: List[Property] = ObjectP.properties
  trait Properties extends ObjectP.Properties

  implicit def toNode[T](geoWithin: GeoWithin[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(ObjectP.keys.value, ClassType.valueToOntologyResource(geoWithin.pvalue), geoWithin.pvalue)
    node
  }
}

case class GeoWithin[T](pvalue: T)(implicit helper: ObjectHelper[T]) extends ObjectP[T] {
  def assert(avalue: Any): Boolean = helper.within(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"within($pvalue)"
}
