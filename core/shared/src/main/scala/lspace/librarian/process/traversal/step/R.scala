package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.datatype.{DataType, ListType}
import lspace.librarian.structure._
import lspace.NS.types

object R
    extends StepDef("R", "An r-step selects resources to traverse from.", () => ResourceStep.ontology :: Nil)
    with StepWrapper[R] {

  def toStep(node: Node): R = R(node.outE(R.keys.resource).map(_.to))

  object keys extends ResourceStep.Properties {
    object resource
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/R/resource",
          "resource",
          "A resource",
          `@range` = () => ListType(DataType.default.`@url` :: Nil) :: Nil
        )
    val resourceUrl: TypedProperty[List[Any]] = resource + ListType(Nil)
  }
  override lazy val properties: List[Property] = keys.resource.property :: ResourceStep.properties
  trait Properties extends ResourceStep.Properties {
    val resource = keys.resource
//    val resourceUrl = keys.resourceUrl
  }

  implicit def toNode(r: R): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    if (r.resources.nonEmpty) node.addOut(keys.resourceUrl, r.resources)
//    r.resources.foreach(
//      v =>
//        node.addOut(
//          keys.resource,
//          v match {
//            case node: Node       => DataType.default.`@nodeURL`.asInstanceOf[ClassType[Any]]
//            case edge: Edge[_, _] => DataType.default.`@edgeURL`.asInstanceOf[ClassType[Any]]
//            case value: Value[_]  => value.label.asInstanceOf[ClassType[Any]]
//          },
//          v
//      ))
    node
  }
}

case class R(resources: List[Resource[_]]) extends ResourceStep {

  def toNode: Node                 = this
  override def prettyPrint: String = "R(" + resources.map(_.iri).mkString(", ") + ")"
}
