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

  def wrap(node: Node): N = node match {
    case node: N => node
    case _       => new N(node.out(keys.nodeUrl), node)
  }

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

  def apply(nodes: List[Node] = List()): N = {
    val node = DetachedGraph.nodes.create(ontology)

    //    println(nodes.map(_.iri).mkString(" >> "))
    nodes.foreach(node.addOut(keys.node, _))
    //    if (nodes.lengthCompare(1) > 0) node.property(V.keys.nodeUrl, nodes.head, nodes.tail: _*)
    //    if (nodes.nonEmpty) property(V.keys.nodeUrl, nodes.head)
    N(nodes, node)
  }
}

case class N private (resources: List[Node], override val value: Node) extends WrappedNode(value) with ResourceStep {
  //  def nodes: List[Node] = property(V.keys.nodeUrl)
  override def prettyPrint: String = "N(" + resources.map(_.iri).mkString(", ") + ")"
}
