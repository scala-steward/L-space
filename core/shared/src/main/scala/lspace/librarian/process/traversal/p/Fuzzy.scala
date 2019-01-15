package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal.{PredicateDef, PredicateWrapper, SeqP}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

object Fuzzy extends PredicateDef("Fuzzy", `@extends` = () => List(SeqP.ontology)) with PredicateWrapper[Fuzzy[_]] {

  def toP(node: Node): Fuzzy[_] = {
    val (pvalue, helper) = StringHelper map node.out(SeqP.keys.value).head
    Fuzzy(pvalue)(helper)
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

case class Fuzzy[+T](pvalue: T)(implicit helper: StringHelper[T]) extends SeqP[T] {
  def assert(avalue: Any): Boolean = helper.fuzzy(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"fuzzy($pvalue)"
}
