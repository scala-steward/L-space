package lspace.librarian.process.traversal.step

import lspace.NS.types
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Is extends StepDef("Is", "An is-step ..", () => FilterStep.ontology :: Nil) with StepWrapper[Is] {

  def wrap(node: Node): Is = node match {
    case node: Is => node
    case _        => Is(node.out(Is.keys.predicateUrl).map(P.wrap), node)
  }

  object keys {
    object predicate
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Is/Predicate",
          "Predicate",
          "A Predicate",
          container = types.`@list` :: Nil,
          `@range` = () => P.ontology :: Nil
        )
    val predicateUrl: TypedProperty[Node] = predicate.property + P.ontology
  }
  override lazy val properties: List[Property] = keys.predicate :: FilterStep.properties
  trait Properties extends FilterStep.Properties {
    val predicate    = keys.predicate
    val predicateUrl = keys.predicateUrl
  }

  def apply(predicates: List[P[_]]): Is = {
    val node = DetachedGraph.nodes.create(ontology)

    predicates.foreach(node.addOut(keys.predicate, _))

    Is(predicates, node)
  }

}

case class Is private (predicate: List[P[_]], override val value: Node) extends WrappedNode(value) with FilterStep {
  override def prettyPrint: String = "is(P." + predicate.head.prettyPrint + ")"
}
