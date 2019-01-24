package lspace.librarian.process.traversal.step

import lspace.librarian.datatype.DataType
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

object As
    extends StepDef(
      "As",
      "An as-step marks the preliminary result so it can be referred to (gathered) further down the traversal.")
    with StepWrapper[As[_ <: Any, String]] {

  /*  import shapeless._
  import shapeless.ops.hlist._
  import syntax.singleton._

  class NamedLabel[S <: String](val name: S)
  object NamedLabel {
    implicit def _fromString(name: String)                    = new NamedLabel(name.narrow)
    implicit def _toString(namedLabel: NamedLabel[_]): String = namedLabel.name.toString
//    def apply(name: String)               = new NamedLabel(name.narrow)
  }*/

  def toStep(node: Node): As[_ <: Any, String] = {
    As[Any, String](node.out(As.keys.nameString).head)
  }

  object keys extends Step.Properties {
    object name
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/As/name",
          "name",
          "A named step-label which can be referred to further in the traversal",
          `@range` = () => DataType.default.`@string` :: Nil
        )
    val nameString: TypedProperty[String] = name.property + DataType.default.`@string`
  }
  override lazy val properties: List[Property] = keys.name :: Step.properties
  trait Properties extends Step.Properties {
    val name       = keys.name
    val nameString = keys.nameString
  }

  implicit def toNode(as: As[_ <: Any, _ <: String]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(keys.nameString, as.label)
    node
  }
}

case class As[T, name <: String](label: name) extends Step {
  def _maphelper: T = List[T]().head

  def toNode: Node                 = this
  override def prettyPrint: String = "as(" + label + ")"
}
