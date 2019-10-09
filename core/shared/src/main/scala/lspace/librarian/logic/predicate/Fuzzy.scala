package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object Fuzzy extends PredicateDef("Fuzzy", `@extends` = List(SeqP.ontology)) with PredicateWrapper[Fuzzy[_]] {

  def toP(node: Node): Fuzzy[_] = {
    val pvalue = node.out(SeqP.keys.value).head
    Fuzzy(pvalue)
  }

  object keys extends SeqP.Properties
  override lazy val properties: List[Property] = SeqP.properties
  trait Properties extends SeqP.Properties

  implicit def toNode[T](p: Fuzzy[T]): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.value, ClassType.detect(p.pvalue), p.pvalue)
    } yield node
  }
}

case class Fuzzy[+T](pvalue: T) extends SeqP[T] {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"fuzzy($pvalue)"
}
