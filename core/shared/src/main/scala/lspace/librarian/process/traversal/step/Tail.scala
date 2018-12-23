package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Tail
    extends StepDef("Tail", "A tail-step limits the traversal to last n-results.", () => ClipStep.ontology :: Nil)
    with StepWrapper[Tail] {

  def wrap(node: Node): Tail = node match {
    case node: Tail => node
    case _          => Tail(node.out(Tail.keys.maxInt).head, node)
  }

  object keys extends ClipStep.Properties {
    object max
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Tail/max",
          "max",
          "The maximum number of tail-results",
          `@range` = () => DataType.default.`@string` :: Nil
        )
    val maxInt: TypedProperty[Int] = max.property + DataType.default.`@int`
  }
  override lazy val properties: List[Property] = keys.max :: ClipStep.properties
  trait Properties extends ClipStep.Properties {
    val max    = keys.max
    val maxInt = keys.maxInt
  }

  def apply(max: Int): Tail = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.maxInt, max)
    Tail(max, node)
  }
}

case class Tail private (max: Int, override val value: Node) extends WrappedNode(value) with ClipStep {
  override def prettyPrint: String = s"tail($max)"
}
