package lspace.librarian.process.traversal.step

import lspace.NS.types
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.datatype.{DataType, ListType}
import lspace.librarian.structure._

object N
    extends StepDef("N", "An n-step selects nodes to traverse from.", () => ResourceStep.ontology :: Nil)
    with StepWrapper[N] {

  def toStep(node: Node): N = N(node.out(keys.nodeUrl).take(1).flatten)

  object keys extends ResourceStep.Properties {
    object node
        extends Property.PropertyDef(
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

  implicit def toNode(n: N): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    if (n.nodes.nonEmpty) node.addOut(keys.nodeUrl, n.nodes)
    node
  }
}

case class N(nodes: List[Node] = List()) extends ResourceStep {

  def toNode: Node                 = this
  override def prettyPrint: String = "N(" + nodes.map(_.iri).mkString(", ") + ")"
}
