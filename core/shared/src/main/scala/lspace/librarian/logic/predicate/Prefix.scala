package lspace.librarian.logic.predicate

import lspace.librarian.logic.predicate.P._
import lspace.librarian.logic.predicate.{PredicateDef, PredicateWrapper, SeqP}
import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Prefix extends PredicateDef("Prefix", `@extends` = () => List(SeqP.ontology)) with PredicateWrapper[Prefix[_]] {

  def toP(node: Node): Prefix[_] = {

    val pvalue = node.out(SeqP.keys.value).head
    Prefix(pvalue)
  }

  object keys extends SeqP.Properties
  override lazy val properties: List[Property] = SeqP.properties
  trait Properties extends SeqP.Properties

  implicit def toNode[T](prefix: Prefix[T]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(SeqP.keys.value, ClassType.valueToOntologyResource(prefix.pvalue), prefix.pvalue)
    node
  }
}

case class Prefix[+T](pvalue: T) extends SeqP[T] {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"prefix($pvalue)"
}
