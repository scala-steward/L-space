package lspace.librarian.logic.predicate

import lspace.datatype.ListType
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object And extends PredicateDef("And") with PredicateWrapper[And] {

  def toP(node: Node): And = {
    val predicates = node.out(keys.predicateP).flatten.map(P.toP)
    And(predicates.asInstanceOf[List[P[Any]]])
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

  implicit def toNode(p: And): Task[Node] = {
    for {
      node       <- DetachedGraph.nodes.create(ontology)
      predicates <- Task.gather(p.predicate.map(_.toNode))
      _          <- node.addOut(keys.predicate, predicates)
    } yield node
  }

  implicit class WithAndPredicate(and: And) {
    def &&[T, PR[Z] <: P[Z]](predicate: PR[T]*): And = And(and.predicate ::: predicate.toList)
    def ||[T, PR[Z] <: P[Z]](predicate: PR[T]*): Or  = Or((and: P[Any]) :: predicate.toList)
  }
}
case class And(predicate: List[P[Any]]) extends P[Any] {
  def _pvalue: Any = predicate.map(_._pvalue)

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"and(${predicate.map(_.prettyPrint).mkString(", ")})"
}
