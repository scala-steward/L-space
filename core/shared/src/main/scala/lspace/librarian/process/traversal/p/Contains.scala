package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

object Contains
    extends PredicateDef("Contains", `@extends` = () => List(CollectionP.ontology))
    with PredicateWrapper[Contains[_]] {
  trait Helper[T] {
    def contains(avalue: Any, pvalue: T): Boolean
  }

  def toP(node: Node): Contains[_] = {
    val (pvalue, helper) = CollectionHelper map node.out(CollectionP.keys.value).head
    Contains(pvalue)(helper)
  }

  object keys extends CollectionP.Properties
  override lazy val properties: List[Property] = CollectionP.properties
  trait Properties extends CollectionP.Properties

  implicit def toNode[T](contains: Contains[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(CollectionP.keys.value, ClassType.valueToOntologyResource(contains.pvalue), contains.pvalue)
    node
  }
}

case class Contains[+T](pvalue: T)(implicit helper: CollectionHelper[T]) extends CollectionP[T] {
  def assert(avalue: Any): Boolean = helper.contains(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"contains($pvalue)"
}
