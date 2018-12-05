package lspace.librarian.process.traversal.step

import lspace.NS.types
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.types._

object Limit extends StepDef("Limit") with StepWrapper[Limit] {

  def wrap(node: Node): Limit = node match {
    case node: Limit => node
    case _           => Limit(node.out(Limit.keys.maxInt).head, node)
  }

  object keys {
    private val maxNode =
      MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/step/Limit/max")
    maxNode.addLabel(Property.ontology)
    maxNode --- Property.default.`@label` --> "max" --- Property.default.`@language` --> "en"
    maxNode --- Property.default.`@comment` --> "The maximum number of results" --- Property.default.`@language` --> "en"
    maxNode --- Property.default.`@range` --> DataType.default.`@int`

    lazy val max: Property         = Property(maxNode)
    val maxInt: TypedProperty[Int] = max + DataType.default.`@int`
  }

  def apply(max: Int): Limit = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.maxInt, max)
    Limit(max, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.max
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Limit private (max: Int, override val value: Node) extends WrappedNode(value) with ClipStep {
  override def prettyPrint: String = "limit(" + max + ")"
}
