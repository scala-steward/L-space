package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._

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

case class Within[+T](pvalue: T) extends CollectionP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"within(${pvalue.toString})"
}
