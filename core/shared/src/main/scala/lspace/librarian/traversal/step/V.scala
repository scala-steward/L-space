package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import lspace.datatype.{DataType, ListType}

object V
    extends StepDef("V", "An v-step selects values to traverse from.", () => ResourceStep.ontology :: Nil)
    with StepWrapper[V] {

  def toStep(node: Node): V = V(node.out(keys.valueUrl).take(1).flatten)

  object keys extends ResourceStep.Properties {
    object value
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/V/value",
          "value",
          "A value",
          `@range` = () => ListType(DataType.default.`@datatype` :: Nil) :: Nil
        )
    val valueUrl: TypedProperty[List[Any]] = value.property + ListType(DataType.default.`@datatype` :: Nil)
  }
  override lazy val properties: List[Property] = keys.value :: ResourceStep.properties
  trait Properties extends ResourceStep.Properties {
    val value    = keys.value
    val valueUrl = keys.valueUrl
  }

  implicit def toNode(v: V): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(keys.valueUrl, v.values)
//    v.values
//      .map(v => node.addOut(keys.value, ClassType.valueToOntologyResource(v), v))
//      .map(_.to)
//      .asInstanceOf[List[Value[_]]]
    node
  }
}

case class V(values: List[_] = List()) extends ResourceStep {

  def toNode: Node = this
  override def prettyPrint: String =
    "V(" + values.map {
      case resource: Resource[_] => resource.value
      case v                     => v
    } + ")"
}
