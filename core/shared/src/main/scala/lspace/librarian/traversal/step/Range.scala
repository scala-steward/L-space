package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.datatype.DataType
import lspace.structure._

object Range extends StepDef("Range", "A range ..", () => ClipStep.ontology :: Nil) with StepWrapper[Range] {

  def toStep(node: Node): Range =
    Range(node.out(Range.keys.lowInt).take(1).head, node.out(Range.keys.highInt).take(1).head)

  object keys extends FilterStep.Properties {
    object low
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Range/low",
          "low",
          "The lower result-index to start from",
          `@range` = () => DataType.default.`@int` :: Nil
        )
    val lowInt: TypedProperty[Int] = low.property + DataType.default.`@int`

    object high
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Range/high",
          "high",
          "The higher result-index to start from",
          `@range` = () => DataType.default.`@int` :: Nil
        )
    val highInt: TypedProperty[Int] = high.property + DataType.default.`@int`
  }
  override lazy val properties: List[Property] = keys.low.property :: keys.high.property :: ClipStep.properties
  trait Properties extends FilterStep.Properties {
    val low     = keys.low
    val lowInt  = keys.lowInt
    val high    = keys.high
    val highInt = keys.highInt
  }

  implicit def toNode(range: Range): Node = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.lowInt, range.low)
    node.addOut(keys.highInt, range.high)
    node
  }

}

case class Range(low: Int, high: Int) extends ClipStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = s"range($low, $high)"
}
