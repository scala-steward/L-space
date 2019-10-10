package lspace.librarian.traversal.step

import lspace.librarian.logic.predicate.P
import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object Has
    extends StepDef(
      "Has",
      "A has-step grants the traverser passage if the travers holds a " +
        "resource which satisfies having certains properties (and values)",
      HasStep.ontology :: Nil
    )
    with StepWrapper[Has] {

  def toStep(node: Node): Task[Has] =
    for {
      key <- {
        val name = node
          .outE(keys.key)
          .head
          .to
          .iri
        node.graph.ns.properties //all known Properties are globally available, why look in 'a' graph? E.g. this graph is likely the DetachedGraph
          .get(name)
          .map(_.getOrElse(Property(name)))
      }
      p = node
        .out(keys.predicateUrl)
        .map(P.toP)
        .headOption
    } yield Has(key, p)

  object keys {
    object key
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Has/Key",
          "Key",
          "A key",
          `@range` = Property.ontology :: Nil
        )
    val keyUrl: TypedProperty[Node] = key.property as Property.ontology

    object predicate
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Has/Predicate",
          "Predicate",
          "A Predicate",
          `@range` = P.ontology :: Nil
        )
    val predicateUrl: TypedProperty[Node] = predicate.property as P.ontology
  }
  override lazy val properties: List[Property] = keys.key.property :: keys.predicate.property :: HasStep.properties
  trait Properties extends HasStep.Properties {
    val key          = keys.key
    val keyUrl       = keys.keyUrl
    val predicate    = keys.predicate
    val predicateUrl = keys.predicateUrl
  }

  implicit def toNode(step: Has): Task[Node] = {
    for {
      node       <- DetachedGraph.nodes.create(ontology)
      _          <- node.addOut(keys.key, step.key)
      predicates <- Task.gather(step.predicate.toList.map(_.toNode))
      _          <- Task.gather(predicates.map(node.addOut(keys.predicateUrl, _)))
    } yield node
  }.memoizeOnSuccess

}

case class Has(key: Property, predicate: Option[P[_]] = None) extends HasStep {

  lazy val toNode: Task[Node] = this
  override def prettyPrint: String =
    if (predicate.nonEmpty) s"has(${key.iri}, P.${predicate.head.prettyPrint})"
    else "has(" + key.iri + ")"
}
