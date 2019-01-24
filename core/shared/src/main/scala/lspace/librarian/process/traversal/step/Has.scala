package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types

object Has
    extends StepDef(
      "Has",
      "A has-step grants the traverser passage if the travers holds a " +
        "resource which satisfies having certains properties (and values)",
      () => HasStep.ontology :: Nil
    )
    with StepWrapper[Has] {

  def toStep(node: Node): Has =
    Has(node
          .outE(keys.key)
          .take(1)
          .map(i => node.graph.ns.properties.get(i.inV.iri).get)
          .head,
        node
          .out(keys.predicateUrl)
          .headOption
          .map(P.toP))

  object keys {
    object key
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Has/Key",
          "Key",
          "A key",
          `@range` = () => Property.ontology :: Nil
        )
    val keyUrl: TypedProperty[Node] = key.property + Property.ontology

    object predicate
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Has/Predicate",
          "Predicate",
          "A Predicate",
          `@range` = () => P.ontology :: Nil
        )
    val predicateUrl: TypedProperty[Node] = predicate.property + P.ontology
  }
  override lazy val properties: List[Property] = keys.key.property :: keys.predicate.property :: HasStep.properties
  trait Properties extends HasStep.Properties {
    val key          = keys.key
    val keyUrl       = keys.keyUrl
    val predicate    = keys.predicate
    val predicateUrl = keys.predicateUrl
  }

  implicit def toNode(has: Has): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    val edge = node.addOut(keys.key, has.key)
    has.predicate.map(_.toNode).foreach(node.addOut(keys.predicateUrl, _))
    node
  }

}

case class Has(key: Property, predicate: Option[P[_]] = None) extends HasStep {

  lazy val toNode: Node = this
  override def prettyPrint: String =
    if (predicate.nonEmpty) s"has(${key.iri}, P.${predicate.head.prettyPrint})"
    else "has(" + key.iri + ")"
}
