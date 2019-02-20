package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.datatype.{DataType, ListType}
import lspace.structure._
import lspace.NS.types

object E
    extends StepDef("E", "An e-step selects edges to traverse from.", () => ResourceStep.ontology :: Nil)
    with StepWrapper[E] {

  def toStep(node: Node): E = node match {
    case node: E => node
    case _       => E(node.out(keys.edgeUrl).take(1).flatten)
  }

  object keys extends ResourceStep.Properties {
    object edge
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/E/edge",
          "edge",
          "An edge",
          `@range` = () => ListType(DataType.default.`@edgeURL` :: Nil) :: Nil
        )
    val edgeUrl: TypedProperty[List[Edge[Any, Any]]] = edge.property + ListType(DataType.default.`@edgeURL` :: Nil)
  }
  override lazy val properties: List[Property] = keys.edge :: ResourceStep.properties
  trait Properties extends ResourceStep.Properties {
    val edge    = keys.edge
    val edgeUrl = keys.edgeUrl
  }

  implicit def toNode(e: E): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(keys.edgeUrl, e.links)
//    e.links.foreach(node.addOut(keys.edge, _))
    node
  }

}

case class E(links: List[Edge[Any, Any]] = List()) extends ResourceStep {

  def toNode: Node                 = this
  override def prettyPrint: String = "E(" + links.map(_.id).mkString(", ") + ")"
}
