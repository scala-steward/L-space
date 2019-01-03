package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.types._

object V
    extends StepDef("V", "An v-step selects values to traverse from.", () => ResourceStep.ontology :: Nil)
    with StepWrapper[V] {

  def toStep(node: Node): V = V(node.out(keys.valueUrl))

  object keys extends ResourceStep.Properties {
    object value
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/V/value",
          "value",
          "A value",
          container = types.`@list` :: Nil,
          `@range` = () => DataType.default.`@nodeURL` :: Nil
        )
    val valueUrl: TypedProperty[Value[Any]] = value.property + DataType.default.`@valueURL`
  }
  override lazy val properties: List[Property] = keys.value :: ResourceStep.properties
  trait Properties extends ResourceStep.Properties {
    val value    = keys.value
    val valueUrl = keys.valueUrl
  }

  implicit def toNode(v: V): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    v.values
      .map(v => node.addOut(keys.value, ClassType.valueToOntologyResource(v), v))
      .map(_.to)
      .asInstanceOf[List[Value[_]]]
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
