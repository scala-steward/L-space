package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

object Intersect
    extends PredicateDef("Intersect", `@extends` = () => CollectionP.ontology :: Nil)
    with PredicateWrapper[Intersect[_]] {

  def toP(node: Node): Intersect[_] = {
    val (pvalue, helper) = CollectionHelper map node.out(CollectionP.keys.value).head
    Intersect(pvalue)(helper)
  }

  object keys extends CollectionP.Properties
  override lazy val properties: List[Property] = CollectionP.properties
  trait Properties extends CollectionP.Properties

  implicit def toNode[T](intersect: Intersect[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(CollectionP.keys.value, ClassType.valueToOntologyResource(intersect.pvalue), intersect.pvalue)
    node
  }
}

case class Intersect[+T](pvalue: T)(implicit helper: CollectionHelper[T]) extends CollectionP[T] {
  def assert(avalue: Any): Boolean = helper.intersect(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"intersect($pvalue)"
}
