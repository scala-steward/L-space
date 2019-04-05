package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure.{Node, Property, PropertyDef, TypedProperty}
import lspace.NS.types
import lspace.librarian.logic.predicate.P
import monix.eval.Task

object HasNot
    extends StepDef(
      "HasNot",
      "A hasNot-step grants the traverser passage if the traverser holds a " +
        "resource which does not satisfy certains properties (and values)",
      () => HasStep.ontology :: Nil
    )
    with StepWrapper[HasNot] {

  def toStep(node: Node): Task[HasNot] =
    for {
      key <- node.graph.ns.properties
        .get(
          node
            .outE(keys.key)
            .head
            .iri)
        .map(_.get)
      p = node
        .out(keys.predicateUrl)
        .map(P.toP)
        .headOption
    } yield HasNot(key, p)

  object keys {
    object key
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/HasNot/Key",
          "Key",
          "A key",
          `@range` = () => Property.ontology :: Nil
        )
    val keyUrl: TypedProperty[Node] = key.property + Property.ontology

    object predicate
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/HasNot/Predicate",
          "Predicate",
          "A Predicate",
          container = types.`@list` :: Nil,
          `@range` = () => P.ontology :: Nil
        )
    val predicateUrl: TypedProperty[Node] = key.property + P.ontology
  }
  override lazy val properties: List[Property] = keys.key.property :: keys.predicate.property :: HasStep.properties
  trait Properties extends HasStep.Properties {
    val key          = keys.key
    val keyUrl       = keys.keyUrl
    val predicate    = keys.predicate
    val predicateUrl = keys.predicateUrl
  }

  implicit def toNode(step: HasNot): Task[Node] = {
    for {
      node       <- DetachedGraph.nodes.create(ontology)
      _          <- node.addOut(keys.key, step.key)
      predicates <- Task.gather(step.predicate.toList.map(_.toNode))
      _          <- Task.gather(predicates.map(node.addOut(keys.predicateUrl, _)))
    } yield node
  }.memoizeOnSuccess

}

case class HasNot(key: Property, predicate: Option[P[_]] = None) extends HasStep {

  lazy val toNode: Task[Node] = this
  override def prettyPrint: String =
    if (predicate.nonEmpty) s"hasNot(${key.iri}, P.${predicate.head.prettyPrint})"
    else "hasNot(" + key.iri + ")"
}
