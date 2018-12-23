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
    with StepWrapper[As[Any, String]] {

  /*  import shapeless._
  import shapeless.ops.hlist._
  import syntax.singleton._

  class NamedLabel[S <: String](val name: S)
  object NamedLabel {
    implicit def _fromString(name: String)                    = new NamedLabel(name.narrow)
    implicit def _toString(namedLabel: NamedLabel[_]): String = namedLabel.name.toString
//    def apply(name: String)               = new NamedLabel(name.narrow)
  }*/

  def wrap(node: Node): As[Any, String] = node match {
    case node: As[Any, String] => node
    case _                     => As[Any, String](node.out(As.keys.nameString).head, node)
  }

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

  def apply[T, Tname <: String](label: Tname): As[T, Tname] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.nameString, label.toString)
    As[T, Tname](label, node)
  }
}

case class As[T, name <: String] private (label: name, override val value: Node)
    extends WrappedNode(value)
    with TraverseStep {
  def _maphelper = List[T]().head

  override def prettyPrint: String = "as(" + label + ")"
}
