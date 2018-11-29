package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.types._

object R extends StepDef("R") with StepWrapper[R] {

  def wrap(node: Node): R = node match {
    case node: R => node
    case _       => R(node.out(R.keys.resourceUrl), node)
  }

  object keys {
    private val resourceNode =
      MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/step/R/resource")
    resourceNode.addLabel(Property.ontology)
    resourceNode --- Property.default.`@label` --> "resource" --- Property.default.`@language` --> "en"
    resourceNode --- Property.default.`@comment` --> "A resource" --- Property.default.`@language` --> "en"
    resourceNode --- Property.default.`@container` --> types.`@list`
    resourceNode --- Property.default.`@range` --> DataType.default.edgeURLType

    lazy val resource: Property         = Property(resourceNode)
    val resourceUrl: TypedProperty[Any] = resource + DataType.default.uRLType
  }

  def apply(values: List[Resource[Any]]): R = {
    val node = DetachedGraph.nodes.create(ontology)

    values.foreach(
      v =>
        node.addOut(
          keys.resource,
          v match {
            case node: Node       => DataType.default.nodeURLType.asInstanceOf[ClassType[Any]]
            case edge: Edge[_, _] => DataType.default.edgeURLType.asInstanceOf[ClassType[Any]]
            case value: Value[_]  => value.label.asInstanceOf[ClassType[Any]]
          },
          v
      ))
    //    if (nodes.lengthCompare(1) > 0) node.property(V.keys.nodeUrl, nodes.head, nodes.tail: _*)
    //    if (nodes.nonEmpty) property(V.keys.nodeUrl, nodes.head)
    R(values, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.resource
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class R private (resources: List[Any], override val value: Node) extends WrappedNode(value) with ResourceStep {
  //  def valueResource: List[ValueResource[Any]] = property(VR.keys.valueUrl)
}
