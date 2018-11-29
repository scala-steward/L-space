package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.types._

object V extends StepDef("V") with StepWrapper[V] {

  def wrap(node: Node): V = node match {
    case node: V => node
    case _       => new V(node.out(keys.valueUrl), node)
  }

  object keys {
    private val valueNode =
      MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/step/V/value")
    valueNode.addLabel(Property.ontology)
    valueNode --- Property.default.`@label` --> "value" --- Property.default.`@language` --> "en"
    valueNode --- Property.default.`@comment` --> "A value" --- Property.default.`@language` --> "en"
    valueNode --- Property.default.`@container` --> types.`@list`
    valueNode --- Property.default.`@range` --> DataType.default.valueURLType

    lazy val value: Property                = Property(valueNode)
    val valueUrl: TypedProperty[Value[Any]] = value + DataType.default.valueURLType
  }

  def apply(values: List[Any] = List()): V = {
    val node = DetachedGraph.nodes.create(ontology)

    //    println(nodes.map(_.iri).mkString(" >> "))

    val _values = values
      .map(v => node.addOut(keys.value, ClassType.valueToOntologyResource(v), v))
      .map(_.to)
      .asInstanceOf[List[Value[_]]]
    //    if (nodes.lengthCompare(1) > 0) node.property(V.keys.nodeUrl, nodes.head, nodes.tail: _*)
    //    if (nodes.nonEmpty) property(V.keys.nodeUrl, nodes.head)
//    V(values.map(v => DetachedGraph.values.create(v)), node)
    V(_values, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.value
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class V private (resources: List[Value[_]], override val value: Node)
    extends WrappedNode(value)
    with ResourceStep {
  //  def nodes: List[Node] = property(V.keys.nodeUrl)
  override def prettyPrint: String = "V(" + resources.map(_.value) + ")"
}
