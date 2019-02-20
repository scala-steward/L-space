package lspace.librarian.logic.predicate

import lspace.librarian.logic.predicate.P._
import lspace.librarian.logic.predicate._
import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Contains
    extends PredicateDef("Contains", `@extends` = () => List(CollectionP.ontology))
    with PredicateWrapper[Contains[_]] {
  trait Helper[T] {
    def contains(avalue: Any, pvalue: T): Boolean
  }

  def toP(node: Node): Contains[_] = {
    val pvalue = node.out(CollectionP.keys.value).head
    Contains(pvalue)
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

case class Contains[+T](pvalue: T) extends CollectionP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"contains($pvalue)"
}
