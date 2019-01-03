package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object As
    extends StepDef(
      "As",
      "An as-step marks the preliminary result so it can be referred to (gathered) further down the traversal.",
      () => TraverseStep.ontology :: Nil)
    with StepWrapper[As[_ <: Any, _ <: String]] {

  /*  import shapeless._
  import shapeless.ops.hlist._
  import syntax.singleton._

  class NamedLabel[S <: String](val name: S)
  object NamedLabel {
    implicit def _fromString(name: String)                    = new NamedLabel(name.narrow)
    implicit def _toString(namedLabel: NamedLabel[_]): String = namedLabel.name.toString
//    def apply(name: String)               = new NamedLabel(name.narrow)
  }*/

  def toStep(node: Node): As[_ <: Any, _ <: String] =
    As[Any, String](node.out(As.keys.nameString).head)

  object keys extends TraverseStep.Properties {
    object name
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/As/name",
          "name",
          "A named step-label which can be referred to further in the traversal",
          `@range` = () => DataType.default.`@string` :: Nil
        )
    val nameString: TypedProperty[String] = name.property + DataType.default.`@string`
  }
  override lazy val properties: List[Property] = keys.name :: TraverseStep.properties
  trait Properties extends TraverseStep.Properties {
    val name       = keys.name
    val nameString = keys.nameString
  }

  implicit def toNode(as: As[_ <: Any, _ <: String]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(keys.nameString, as.label.toString)
    node
  }
}

case class As[T, name <: String](label: name) extends TraverseStep {
  def _maphelper = List[T]().head

  def toNode: Node                 = this
  override def prettyPrint: String = "as(" + label + ")"
}
