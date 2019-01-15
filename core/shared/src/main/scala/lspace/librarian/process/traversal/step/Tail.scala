package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.datatype.DataType
import lspace.librarian.structure._

object Tail
    extends StepDef("Tail", "A tail-step limits the traversal to last n-results.", () => ClipStep.ontology :: Nil)
    with StepWrapper[Tail] {

  def toStep(node: Node): Tail = Tail(node.out(Tail.keys.maxInt).head)

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

  implicit def toNode(tail: Tail): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(keys.maxInt, tail.max)
    node
  }
}

case class Tail(max: Int) extends ClipStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"tail($max)"
}
