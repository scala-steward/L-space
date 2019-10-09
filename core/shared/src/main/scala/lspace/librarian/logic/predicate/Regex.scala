package lspace.librarian.logic.predicate

import lspace.datatype.DataType
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object Regex extends PredicateDef("Regex", `@extends` = List(SeqP.ontology)) with PredicateWrapper[Regex] {

  def toP(node: Node): Regex = Regex(node.out(SeqP.keys.value as DataType.default.`@string`).head.r)

  object keys extends SeqP.Properties
  override lazy val properties: List[Property] = SeqP.properties
  trait Properties extends SeqP.Properties

  implicit def toNode(regex: Regex): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(SeqP.keys.value, regex.pvalue.regex)
    } yield node
  }
}

case class Regex(pvalue: scala.util.matching.Regex) extends SeqP[scala.util.matching.Regex] {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"regex($pvalue)"
}
