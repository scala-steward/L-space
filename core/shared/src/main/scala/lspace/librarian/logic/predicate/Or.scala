package lspace.librarian.logic.predicate

import lspace.datatype.ListType
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object Or extends PredicateDef("Or") with PredicateWrapper[Or] {

  def toP(node: Node): Or = {
    Or(node.out(keys.predicateP).flatten.map(P.toP))
  }

  object keys extends P.Properties {
    object predicate
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/p/value",
          "value",
          "Any value",
          `@range` = () => ListType(P.ontology) :: Nil
        ) {}
    lazy val predicateP: TypedProperty[List[Node]] = predicate as ListType(P.ontology)
  }
  override lazy val properties: List[Property] = keys.predicate.property :: P.properties
  trait Properties extends P.Properties {
    lazy val predicate  = keys.predicate
    lazy val predicateP = keys.predicateP
  }

  implicit def toNode(p: Or): Task[Node] = {
    for {
      node       <- DetachedGraph.nodes.create(ontology)
      predicates <- Task.gather(p.predicate.map(_.toNode))
      _          <- node.addOut(keys.predicate, predicates)
    } yield node
  }

  implicit class WithOrPredicate(or: Or) {
    def &&[T, PR[Z] <: P[Z]](predicate: PR[T]*): And = And((or: P[Any]) :: predicate.toList)
    def ||[T, PR[Z] <: P[Z]](predicate: PR[T]*): Or  = Or(or.predicate ::: predicate.toList)
  }
}

case class Or(predicate: List[P[_]]) extends P[Any] {
  def _pvalue: Any = predicate.map(_._pvalue)

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"or(${predicate.map(_.prettyPrint).mkString(", ")})"
}
