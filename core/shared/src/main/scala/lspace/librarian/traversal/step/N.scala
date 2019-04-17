package lspace.librarian.traversal.step

import lspace.NS.types
import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.datatype.{DataType, ListType}
import lspace.structure._
import monix.eval.Task

object N
    extends StepDef("N", "An n-step selects nodes to traverse from.", () => ResourceStep.ontology :: Nil)
    with StepWrapper[N] {

  def toStep(node: Node): Task[N] = Task.now(N(node.out(keys.nodeUrl).take(1).flatten))

  object keys extends ResourceStep.Properties {
    object node
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/N/node",
          "node",
          "A node",
          `@range` = () => ListType(DataType.default.`@nodeURL` :: Nil) :: Nil
        )
    val nodeUrl: TypedProperty[List[Node]] = node.property + ListType(DataType.default.`@nodeURL` :: Nil)
  }
  override lazy val properties: List[Property] = keys.node :: ResourceStep.properties
  trait Properties extends ResourceStep.Properties {
    val node    = keys.node
    val nodeUrl = keys.nodeUrl
  }

  implicit def toNode(step: N): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- if (step.nodes.nonEmpty) node.addOut(keys.nodeUrl, step.nodes) else Task.unit
    } yield node
  }.memoizeOnSuccess
}

case class N(nodes: List[Node] = List()) extends ResourceStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "N(" + nodes.map(_.iri).mkString(", ") + ")"
}
