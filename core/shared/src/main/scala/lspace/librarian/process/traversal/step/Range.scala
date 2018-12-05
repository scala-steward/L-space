package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.types._

object Range extends StepDef("Range") with StepWrapper[Range] {

  def wrap(node: Node): Range = node match {
    case node: Range => node
    case _ =>
      Range(node.out(Range.keys.lowInt).take(1).head, node.out(Range.keys.highInt).take(1).head, node)
  }

  object keys {
    private val lowNode =
      MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/step/Range/low")
    lowNode.addLabel(Property.ontology)
    lowNode --- Property.default.`@label` --> "low" --- Property.default.`@language` --> "en"
    lowNode --- Property.default.`@comment` --> "The lower result-index to start from" --- Property.default.`@language` --> "en"
    lowNode --- Property.default.`@range` --> DataType.default.`@int`
    lazy val low: Property         = Property(lowNode)
    val lowInt: TypedProperty[Int] = low + DataType.default.`@int`

    private val highNode =
      MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/step/Range/high")
    highNode.addLabel(Property.ontology)
    highNode --- Property.default.`@label` --> "high" --- Property.default.`@language` --> "en"
    highNode --- Property.default.`@comment` --> "The higher result-index to start from" --- Property.default.`@language` --> "en"
    highNode --- Property.default.`@range` --> DataType.default.`@int`
    lazy val high: Property         = Property(highNode)
    val highInt: TypedProperty[Int] = high + DataType.default.`@int`
  }

  def apply(low: Int, high: Int): Range = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.lowInt, low)
    node.addOut(keys.highInt, high)
    Range(low, high, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.low
  ontologyNode --- Property.default.`@properties` --> keys.high
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Range private (low: Int, high: Int, override val value: Node) extends WrappedNode(value) with ClipStep {
  override def prettyPrint: String = s"range($low, $high)"
}
