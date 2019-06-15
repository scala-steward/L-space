package lspace.librarian.logic.predicate

import lspace.datatype.DataType
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object ContainsRegex
    extends PredicateDef("ContainsRegex", `@extends` = () => List(SeqP.ontology))
    with PredicateWrapper[ContainsRegex] {

  def toP(node: Node): ContainsRegex = ContainsRegex(node.out(SeqP.keys.value as DataType.default.`@string`).head.r)

  object keys extends SeqP.Properties
  override lazy val properties: List[Property] = SeqP.properties
  trait Properties extends SeqP.Properties

  implicit def toNode[T](p: ContainsRegex): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.value, ClassType.valueToOntologyResource(p.pvalue), p.pvalue)
    } yield node
  }
}

case class ContainsRegex(pvalue: scala.util.matching.Regex) extends SeqP[scala.util.matching.Regex] {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"containsRegex($pvalue)"
}
