package lspace.librarian.process.traversal.step

import lspace.NS.types
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object N extends StepDef("N") with StepWrapper[N] {

  def wrap(node: Node): N = node match {
    case node: N => node
    case _       => new N(node.out(keys.nodeUrl), node)
  }

  object keys {
    private val nodeNode =
      MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/step/N/node")
    nodeNode.addLabel(Property.ontology)
    nodeNode --- Property.default.`@label` --> "node" --- Property.default.`@language` --> "en"
    nodeNode --- Property.default.`@comment` --> "A node" --- Property.default.`@language` --> "en"
    nodeNode --- Property.default.`@container` --> types.`@list`
    nodeNode --- Property.default.`@range` --> DataType.default.nodeURLType

    lazy val node: Property          = Property(nodeNode)
    val nodeUrl: TypedProperty[Node] = node + DataType.default.nodeURLType
  }

  def apply(nodes: List[Node] = List()): N = {
    val node = DetachedGraph.nodes.create(ontology)

    //    println(nodes.map(_.iri).mkString(" >> "))
    nodes.foreach(node.addOut(keys.node, _))
    //    if (nodes.lengthCompare(1) > 0) node.property(V.keys.nodeUrl, nodes.head, nodes.tail: _*)
    //    if (nodes.nonEmpty) property(V.keys.nodeUrl, nodes.head)
    N(nodes, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.node
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class N private (resources: List[Node], override val value: Node) extends WrappedNode(value) with ResourceStep {
  //  def nodes: List[Node] = property(V.keys.nodeUrl)
  override def prettyPrint: String = "N(" + resources.map(_.iri).mkString(", ") + ")"
}
