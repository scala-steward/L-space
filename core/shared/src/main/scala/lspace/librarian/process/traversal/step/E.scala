package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.types._

object E extends StepDef("E") with StepWrapper[E] {

  def wrap(node: Node): E = node match {
    case node: E => node
    case _       => E(node.out(keys.edgeUrl), node)
  }

  object keys {
    private val edgeNode =
      MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/step/E/edge")
    edgeNode.addLabel(Property.ontology)
    edgeNode --- Property.default.`@label` --> "edge" --- Property.default.`@language` --> "en"
    edgeNode --- Property.default.`@comment` --> "An edge" --- Property.default.`@language` --> "en"
    edgeNode --- Property.default.`@container` --> types.`@list`
    edgeNode --- Property.default.`@range` --> DataType.default.edgeURLType

    lazy val edge: Property                    = Property(edgeNode)
    val edgeUrl: TypedProperty[Edge[Any, Any]] = edge + DataType.default.edgeURLType
  }

  def apply(links: List[Edge[Any, Any]]): E = {
    val node = DetachedGraph.nodes.create(ontology)

    links.foreach(node.addOut(keys.edge, _))
    E(links, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.edge
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class E private (resources: List[Edge[Any, Any]], override val value: Node)
    extends WrappedNode(value)
    with ResourceStep {
  //  def links: List[Property[_, _]] = property(E.keys.linkUrl)
  override def prettyPrint: String = "E(" + resources.map(_.id).mkString(", ") + ")"
}
