package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure.{Node, Property, PropertyDef, TypedProperty}
import lspace.NS.types

object HasNot
    extends StepDef(
      "HasNot",
      "A hasNot-step grants the traverser passage if the traverser holds a " +
        "resource which does not satisfy certains properties (and values)",
      () => HasStep.ontology :: Nil
    )
    with StepWrapper[HasNot] {

  def toStep(node: Node): HasNot =
    HasNot(node
             .outE(keys.key)
             .take(1)
             .map(i => node.graph.ns.properties.cached(i.inV.iri).get)
             .head,
           node
             .out(keys.predicateUrl)
             .headOption
             .map(P.toP))

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

  implicit def toNode(has: HasNot): Node = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.key, has.key)
    has.predicate.foreach(predicate => node.addOut(keys.predicateUrl, predicate.toNode))
    node
  }

}

case class HasNot(key: Property, predicate: Option[P[_]] = None) extends HasStep {

  lazy val toNode: Node = this
  override def prettyPrint: String =
    if (predicate.nonEmpty) s"hasNot(${key.iri}, P.${predicate.head.prettyPrint})"
    else "hasNot(" + key.iri + ")"
}
