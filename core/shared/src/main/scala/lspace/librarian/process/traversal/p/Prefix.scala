package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P._
import lspace.librarian.process.traversal.{PredicateDef, PredicateWrapper, SeqP}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

object Prefix extends PredicateDef("Prefix", `@extends` = () => List(SeqP.ontology)) with PredicateWrapper[Prefix[_]] {

  def toP(node: Node): Prefix[_] = {

    val (pvalue, helper) = SeqHelper map node.out(SeqP.keys.value).head
    Prefix(pvalue)(helper)
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

case class Prefix[+T](pvalue: T)(implicit helper: SeqHelper[T]) extends SeqP[T] {
  def assert(avalue: Any): Boolean = helper.startsWith(avalue, pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"prefix($pvalue)"
}
