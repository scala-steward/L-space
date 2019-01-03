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

  def toP(node: Node): Contains[_] = {
    val (pvalue, helper) = node.out(EqP.keys.value).head match {
      case v: String => v -> Helper.TextHelper
      case _         => throw new Exception("No StringHelper found")
    }
    Contains(pvalue)(helper)
  }

  object keys extends EqP.Properties
  override lazy val properties: List[Property] = EqP.properties
  trait Properties extends EqP.Properties

  implicit def toNode[T](contains: Contains[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(EqP.keys.value, ClassType.valueToOntologyResource(contains.pvalue), contains.pvalue)
    node
  }
}

case class Contains[T](pvalue: T)(implicit helper: EqHelper[T]) extends EqP[T] {
  def assert(avalue: Any): Boolean = helper.contains(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"contains($pvalue)"
}
