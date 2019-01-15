package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

object Disjoint
    extends PredicateDef("Disjoint", `@extends` = () => CollectionP.ontology :: Nil)
    with PredicateWrapper[Disjoint[_]] {

  def toP(node: Node): Disjoint[_] = {
    val (pvalue, helper) = CollectionHelper map node.out(CollectionP.keys.value).head
    Disjoint(pvalue)(helper)
  }

  object keys extends CollectionP.Properties
  override lazy val properties: List[Property] = CollectionP.properties
  trait Properties extends CollectionP.Properties

  implicit def toNode[T](disjoint: Disjoint[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(CollectionP.keys.value, ClassType.valueToOntologyResource(disjoint.pvalue), disjoint.pvalue)
    node
  }
}

case class Disjoint[+T](pvalue: T)(implicit helper: CollectionHelper[T]) extends CollectionP[T] {
  def assert(avalue: Any): Boolean = helper.disjoint(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"disjoint($pvalue)"
}
