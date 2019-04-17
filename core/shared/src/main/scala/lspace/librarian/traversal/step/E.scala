package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.datatype.{DataType, ListType}
import lspace.structure._
import lspace.NS.types
import monix.eval.Task

object E
    extends StepDef("E", "An e-step selects edges to traverse from.", () => ResourceStep.ontology :: Nil)
    with StepWrapper[E] {

  def toStep(node: Node): Task[E] = node match {
    case node: E => Task.now(node)
    case _       => Task.now(E(node.out(keys.edgeUrl).take(1).flatten))
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

  implicit def toNode(step: E): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- if (step.edges.nonEmpty) node.addOut(keys.edgeUrl, step.edges) else Task.unit
    } yield node
  }.memoizeOnSuccess

}

case class E(edges: List[Edge[Any, Any]] = List()) extends ResourceStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "E(" + edges.map(_.id).mkString(", ") + ")"
}
