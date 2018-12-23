package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Max
    extends StepDef("Max",
                    "A max-step finds the traverser with the largest value within all traversers in-scope.",
                    () => ReducingBarrierStep.ontology :: Nil)
    with StepWrapper[Max] {

  def wrap(node: Node): Max = node match {
    case node: Max => node
    case _         => Max(node)
  }

  object keys extends ReducingBarrierStep.Properties
  override lazy val properties: List[Property] = ReducingBarrierStep.properties
  trait Properties extends ReducingBarrierStep.Properties

  def apply(): Max = {
    val node = DetachedGraph.nodes.create(ontology)

    Max(node)
  }

}

case class Max private (override val value: Node) extends WrappedNode(value) with ReducingBarrierStep {
  override def prettyPrint: String = "max"
}
