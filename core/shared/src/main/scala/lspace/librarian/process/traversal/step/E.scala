package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types

object E
    extends StepDef("E", "An e-step selects edges to traverse from.", () => ResourceStep.ontology :: Nil)
    with StepWrapper[E] {

  def toStep(node: Node): E = node match {
    case node: E => node
    case _       => E(node.out(keys.edgeUrl))
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

  implicit def toNode(e: E): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    e.links.foreach(node.addOut(keys.edge, _))
    node
  }

}

case class E(links: List[Edge[Any, Any]] = List()) extends ResourceStep {

  def toNode: Node                 = this
  override def prettyPrint: String = "E(" + links.map(_.id).mkString(", ") + ")"
}
