package lspace.librarian.process.traversal.step

import lspace.NS.types
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object N
    extends StepDef("N", "An n-step selects nodes to traverse from.", () => ResourceStep.ontology :: Nil)
    with StepWrapper[N] {

  def toStep(node: Node): N = N(node.out(keys.nodeUrl))

  object keys extends ResourceStep.Properties {
    object node
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/N/node",
          "node",
          "A node",
          container = types.`@list` :: Nil,
          `@range` = () => DataType.default.`@nodeURL` :: Nil
        )
    val nodeUrl: TypedProperty[Node] = node.property + DataType.default.`@nodeURL`
  }
  override lazy val properties: List[Property] = keys.node :: ResourceStep.properties
  trait Properties extends ResourceStep.Properties {
    val node    = keys.node
    val nodeUrl = keys.nodeUrl
  }

  implicit def toNode(n: N): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    n.nodes.foreach(node.addOut(keys.node, _))
    node
  }
}

case class N(nodes: List[Node] = List()) extends ResourceStep {

  def toNode: Node                 = this
  override def prettyPrint: String = "N(" + nodes.map(_.iri).mkString(", ") + ")"
}
