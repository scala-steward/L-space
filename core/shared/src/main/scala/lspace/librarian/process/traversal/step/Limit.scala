package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Limit
    extends StepDef("Limit", "A limit-step limits the traversal to first n-results.", () => ClipStep.ontology :: Nil)
    with StepWrapper[Limit] {

  def wrap(node: Node): Limit = node match {
    case node: Limit => node
    case _           => Limit(node.out(Limit.keys.maxInt).head, node)
  }

  object keys extends ClipStep.Properties {
    object max
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Limit/max",
          "max",
          "The maximum number of results",
          `@range` = () => DataType.default.`@string` :: Nil
        )
    val maxInt: TypedProperty[Int] = max.property + DataType.default.`@int`
  }
  override lazy val properties: List[Property] = keys.max :: ClipStep.properties
  trait Properties extends ClipStep.Properties {
    val max    = keys.max
    val maxInt = keys.maxInt
  }

  def apply(max: Int): Limit = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.maxInt, max)
    Limit(max, node)
  }

}

case class Limit private (max: Int, override val value: Node) extends WrappedNode(value) with ClipStep {
  override def prettyPrint: String = "limit(" + max + ")"
}
