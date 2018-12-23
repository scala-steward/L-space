package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Count
    extends StepDef("Count",
                    "A count-step counts the number of remaining traversers.",
                    () => ReducingBarrierStep.ontology :: Nil)
    with StepWrapper[Count] {

  def wrap(node: Node): Count = node match {
    case node: Count => node
    case _           => Count(node)
  }

  object keys extends ReducingBarrierStep.Properties
  override lazy val properties: List[Property] = ReducingBarrierStep.properties
  trait Properties extends ReducingBarrierStep.Properties

  def apply(): Count = {
    val node = DetachedGraph.nodes.create(ontology)

    Count(node)
  }

}

case class Count private (override val value: Node) extends WrappedNode(value) with ReducingBarrierStep {
  override def prettyPrint: String = "count()"
}
