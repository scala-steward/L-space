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

  def toP(node: Node): Disjoint[_] = {
    val (pvalue, helper) = ObjectHelper map node.out(ObjectP.keys.value).head
    Disjoint(pvalue)(helper)
  }

  object keys extends ObjectP.Properties
  override lazy val properties: List[Property] = ObjectP.properties
  trait Properties extends ObjectP.Properties

  implicit def toNode[T](disjoint: Disjoint[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(ObjectP.keys.value, ClassType.valueToOntologyResource(disjoint.pvalue), disjoint.pvalue)
    node
  }
}

case class Disjoint[T](pvalue: T)(implicit helper: ObjectHelper[T]) extends ObjectP[T] {
  def assert(avalue: Any): Boolean = helper.disjoint(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"disjoint($pvalue)"
}
