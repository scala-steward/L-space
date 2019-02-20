package lspace.librarian.logic.predicate

import lspace.librarian.logic.predicate.P._
import lspace.librarian.logic.predicate.{PredicateDef, PredicateWrapper, SeqP}
import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Fuzzy extends PredicateDef("Fuzzy", `@extends` = () => List(SeqP.ontology)) with PredicateWrapper[Fuzzy[_]] {

  def toP(node: Node): Fuzzy[_] = {
    val pvalue = node.out(SeqP.keys.value).head
    Fuzzy(pvalue)
  }

  object keys extends SeqP.Properties
  override lazy val properties: List[Property] = SeqP.properties
  trait Properties extends SeqP.Properties

  implicit def toNode[T](fuzzy: Fuzzy[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(SeqP.keys.value, ClassType.valueToOntologyResource(fuzzy.pvalue), fuzzy.pvalue)
    node
  }
}

case class Fuzzy[+T](pvalue: T) extends SeqP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"fuzzy($pvalue)"
}
