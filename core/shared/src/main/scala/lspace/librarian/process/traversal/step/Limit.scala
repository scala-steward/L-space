package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.datatype.DataType
import lspace.librarian.structure._

object Limit
    extends StepDef("Limit", "A limit-step limits the traversal to first n-results.", () => ClipStep.ontology :: Nil)
    with StepWrapper[Limit] {

  def toStep(node: Node): Limit = Limit(node.out(Limit.keys.maxInt).head)

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

  implicit def toNode(limit: Limit): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(keys.maxInt, limit.max)
    node
  }

}

case class Limit(max: Int) extends ClipStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "limit(" + max + ")"
}
