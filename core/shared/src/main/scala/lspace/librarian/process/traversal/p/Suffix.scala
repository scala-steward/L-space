package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal.{PredicateDef, PredicateWrapper, SeqP}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

object Suffix extends PredicateDef("Suffix", `@extends` = () => List(SeqP.ontology)) with PredicateWrapper[Suffix[_]] {

  def toP(node: Node): Suffix[_] = {

    val (pvalue, helper) = SeqHelper map node.out(SeqP.keys.value).head
    Suffix(pvalue)(helper)
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

case class Suffix[+T](pvalue: T)(implicit helper: SeqHelper[T]) extends SeqP[T] {
  def assert(avalue: Any): Boolean = helper.endsWith(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"suffix($pvalue)"
}
