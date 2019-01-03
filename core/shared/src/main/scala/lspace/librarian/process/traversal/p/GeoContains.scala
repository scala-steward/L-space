package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.{EqP, ObjectP, PredicateDef, PredicateWrapper}
import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.types.vector.Geometry

object GeoContains
    extends PredicateDef("GeoContains", `@extends` = () => List(ObjectP.ontology))
    with PredicateWrapper[GeoContains[_]] {

  def toP(node: Node): GeoContains[_] = {
    val (pvalue, helper) = node.out(ObjectP.keys.value).head match {
      case v: Geometry => v -> Helper.GeoHelper
      case _           => throw new Exception("No StringHelper found")
    }
    GeoContains(pvalue)(helper)
  }

  object keys extends ObjectP.Properties
  override lazy val properties: List[Property] = ObjectP.properties
  trait Properties extends ObjectP.Properties

  implicit def toNode[T](geoContains: GeoContains[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(ObjectP.keys.value, ClassType.valueToOntologyResource(geoContains.pvalue), geoContains.pvalue)
    node
  }
}

case class GeoContains[T](pvalue: T)(implicit helper: ObjectHelper[T]) extends ObjectP[T] {
  def assert(avalue: Any): Boolean = helper.contains(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"contains($pvalue)"
}
