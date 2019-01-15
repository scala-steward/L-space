package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P.CollectionHelper
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

object Within
    extends PredicateDef("Within", `@extends` = () => List(CollectionP.ontology))
    with PredicateWrapper[Within[_]] {

  def toP(node: Node): Within[_] = Within(node.out(EqP.keys.value))

  object keys extends CollectionP.Properties
  override lazy val properties: List[Property] = CollectionP.properties
  trait Properties extends CollectionP.Properties

  implicit def toNode[T](within: Within[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(EqP.keys.value, ClassType.valueToOntologyResource(within.pvalue), within.pvalue)
    node
  }
}

case class Within[+T](pvalue: T)(implicit helper: CollectionHelper[T]) extends CollectionP[T] {
  def assert(avalue: Any): Boolean = helper.within(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"within(${pvalue.toString})"
}
