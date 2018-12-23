package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types

object R
    extends StepDef("R", "An r-step selects resources to traverse from.", () => ResourceStep.ontology :: Nil)
    with StepWrapper[R] {

  def wrap(node: Node): R = node match {
    case node: R => node
    case _       => R(node.out(R.keys.resourceUrl), node)
  }

  object keys extends ResourceStep.Properties {
    object resource
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/R/resource",
          "resource",
          "A resource",
          container = types.`@list` :: Nil,
          `@range` = () => DataType.default.`@url` :: Nil
        )
    val resourceUrl: TypedProperty[Any] = resource + DataType.default.`@url`
  }
  override lazy val properties: List[Property] = keys.resource.property :: ResourceStep.properties
  trait Properties extends ResourceStep.Properties {
    val resource    = keys.resource
    val resourceUrl = keys.resourceUrl
  }

  def apply(values: List[Resource[_]]): R = {
    val node = DetachedGraph.nodes.create(ontology)

    values.foreach(
      v =>
        node.addOut(
          keys.resource,
          v match {
            case node: Node       => DataType.default.`@nodeURL`.asInstanceOf[ClassType[Any]]
            case edge: Edge[_, _] => DataType.default.`@edgeURL`.asInstanceOf[ClassType[Any]]
            case value: Value[_]  => value.label.asInstanceOf[ClassType[Any]]
          },
          v
      ))
    //    if (nodes.lengthCompare(1) > 0) node.property(V.keys.nodeUrl, nodes.head, nodes.tail: _*)
    //    if (nodes.nonEmpty) property(V.keys.nodeUrl, nodes.head)
    R(values, node)
  }

}

case class R private (resources: List[Any], override val value: Node) extends WrappedNode(value) with ResourceStep {
  //  def valueResource: List[ValueResource[Any]] = property(VR.keys.valueUrl)
}
