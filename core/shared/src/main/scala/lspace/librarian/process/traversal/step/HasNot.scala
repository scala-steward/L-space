package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure.{Node, Property, TypedProperty}
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault

object HasNot
    extends StepDef(
      "HasNot",
      "A hasNot-step grants the traverser passage if the traverser holds a " +
        "resource which does not satisfy certains properties (and values)",
      () => HasStep.ontology :: Nil
    )
    with StepWrapper[HasNot] {

  def wrap(node: Node): HasNot = node match {
    case node: HasNot => node
    case _ =>
      HasNot(node
               .outE(keys.key)
               .take(1)
               .map(i => node.graph.ns.getProperty(i.inV.iri).get)
               .head,
             node
               .out(keys.predicateUrl)
               .map(P.wrap),
             node)
  }

  object keys {
    object key
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/HasNot/Key",
          "Key",
          "A key",
          `@range` = () => Property.ontology :: Nil
        )
    val keyUrl: TypedProperty[Node] = key.property + Property.ontology

    object predicate
        extends Property.PropertyDef(
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

  def apply(key: Property, predicates: List[P[_]] = List()): HasNot = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.key, key)

    predicates.foreach(node.addOut(keys.predicate, _))

    HasNot(key, predicates, node)
  }

}

case class HasNot private (key: Property, predicate: List[P[_]], override val value: Node)
    extends WrappedNode(value)
    with HasStep {
  override def prettyPrint: String =
    if (predicate.nonEmpty) s"hasNot(${key.iri}, P.${predicate.head.prettyPrint})"
    else "hasNot(" + key.iri + ")"
}
