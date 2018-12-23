package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types

object E
    extends StepDef("E", "An e-step selects edges to traverse from.", () => ResourceStep.ontology :: Nil)
    with StepWrapper[E] {

  def wrap(node: Node): E = node match {
    case node: E => node
    case _       => E(node.out(keys.edgeUrl), node)
  }

  object keys extends ResourceStep.Properties {
    object edge
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/E/edge",
          "edge",
          "An edge",
          container = types.`@list` :: Nil,
          `@range` = () => DataType.default.`@edgeURL` :: Nil
        )
    val edgeUrl: TypedProperty[Edge[Any, Any]] = edge.property + DataType.default.`@edgeURL`
  }
  override lazy val properties: List[Property] = keys.edge :: ResourceStep.properties
  trait Properties extends ResourceStep.Properties {
    val edge    = keys.edge
    val edgeUrl = keys.edgeUrl
  }

  def apply(links: List[Edge[Any, Any]]): E = {
    val node = DetachedGraph.nodes.create(ontology)

    links.foreach(node.addOut(keys.edge, _))
    E(links, node)
  }

}

case class E private (resources: List[Edge[Any, Any]], override val value: Node)
    extends WrappedNode(value)
    with ResourceStep {
  //  def links: List[Property[_, _]] = property(E.keys.linkUrl)
  override def prettyPrint: String = "E(" + resources.map(_.id).mkString(", ") + ")"
}
