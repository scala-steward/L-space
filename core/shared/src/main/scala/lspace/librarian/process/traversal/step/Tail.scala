package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Tail extends StepDef("Tail") with StepWrapper[Tail] {

  def wrap(node: Node): Tail = node match {
    case node: Tail => node
    case _          => Tail(node.out(Tail.keys.maxInt).head, node)
  }

  object keys {
    private val maxNode =
      MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/step/Tail/max")
    maxNode.addLabel(Property.ontology)
    maxNode --- Property.default.`@label` --> "max" --- Property.default.`@language` --> "en"
    maxNode --- Property.default.`@comment` --> "The maximum number of tail-results" --- Property.default.`@language` --> "en"
    maxNode --- Property.default.`@range` --> DataType.default.`@int`

    lazy val max: Property         = Property(maxNode)
    val maxInt: TypedProperty[Int] = max + DataType.default.`@int`
  }

  def apply(max: Int): Tail = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.maxInt, max)
    Tail(max, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.max
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Tail private (max: Int, override val value: Node) extends WrappedNode(value) with ClipStep {
  override def prettyPrint: String = s"tail($max)"
}
