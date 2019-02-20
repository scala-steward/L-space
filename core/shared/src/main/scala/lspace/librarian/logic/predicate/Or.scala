package lspace.librarian.logic.predicate

import lspace.datatype.ListType
import lspace.librarian.logic.predicate.{P, PredicateDef, PredicateWrapper}
import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Or extends PredicateDef("Or", `@extends` = () => List(P.ontology)) with PredicateWrapper[Or] {

  def toP(node: Node): Or = {
    Or(node.out(keys.predicateP).flatten.map(P.toP))
  }

  object keys extends P.Properties {
    object predicate
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/p/value",
          "value",
          "Any value",
          `@range` = () => ListType(P.ontology :: Nil) :: Nil
        ) {}
    lazy val predicateP: TypedProperty[List[Node]] = predicate as ListType(P.ontology :: Nil)
  }
  override lazy val properties: List[Property] = keys.predicate.property :: P.properties
  trait Properties extends P.Properties {
    lazy val predicate  = keys.predicate
    lazy val predicateP = keys.predicateP
  }

  implicit def toNode(eqv: Or): Node = {
    val node           = DetachedGraph.nodes.create(ontology)
    val predicateNodes = eqv.predicate.map(_.toNode)
    node.addOut(keys.predicateP, predicateNodes)
    node
  }

  implicit class WithAndPredicate(or: Or) {
    def &&[T, PR[Z] <: P[Z]](predicate: PR[T]*): And = And((or: P[Any]) :: predicate.toList)
    def ||[T, PR[Z] <: P[Z]](predicate: PR[T]*): Or  = Or(or.predicate ::: predicate.toList)
  }
}

case class Or(predicate: List[P[_]]) extends P[Any] {
  def _pvalue: Any = predicate.map(_._pvalue)

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"or(${predicate.map(_.prettyPrint).mkString(", ")})"
}
