package lspace.librarian.logic.predicate

import lspace.librarian.logic.predicate.P._
import lspace.librarian.logic.predicate._
import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Intersect
    extends PredicateDef("Intersect", `@extends` = () => CollectionP.ontology :: Nil)
    with PredicateWrapper[Intersect[_]] {

  def toP(node: Node): Intersect[_] = {
    val pvalue = node.out(CollectionP.keys.value).head
    Intersect(pvalue)
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

case class Intersect[+T](pvalue: T) extends CollectionP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"intersect($pvalue)"
}
