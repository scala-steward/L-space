package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Suffix extends PredicateDef("Suffix", `@extends` = () => List(SeqP.ontology)) with PredicateWrapper[Suffix[_]] {

  def toP(node: Node): Suffix[_] = {

    val pvalue = node.out(SeqP.keys.value).head
    Suffix(pvalue)
  }

  object keys extends SeqP.Properties
  override lazy val properties: List[Property] = SeqP.properties
  trait Properties extends SeqP.Properties

  implicit def toNode[T](suffix: Suffix[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(SeqP.keys.value, ClassType.valueToOntologyResource(suffix.pvalue), suffix.pvalue)
    node
  }
}

case class Suffix[+T](pvalue: T) extends SeqP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"suffix($pvalue)"
}
