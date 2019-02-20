package lspace.librarian.logic.predicate

import lspace.librarian.logic.predicate.P._
import lspace.librarian.logic.predicate._
import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Disjoint
    extends PredicateDef("Disjoint", `@extends` = () => CollectionP.ontology :: Nil)
    with PredicateWrapper[Disjoint[_]] {

  def toP(node: Node): Disjoint[_] = {
    val pvalue = node.out(CollectionP.keys.value).head
    Disjoint(pvalue)
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

case class Disjoint[+T](pvalue: T) extends CollectionP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"disjoint($pvalue)"
}
