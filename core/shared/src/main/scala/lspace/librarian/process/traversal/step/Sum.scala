package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Sum
    extends StepDef("Sum",
                    "A sum-step calculates the summed value of all traversers in-scope.",
                    () => ReducingBarrierStep.ontology :: Nil)
    with StepWrapper[Sum] {

  def wrap(node: Node): Sum = node match {
    case node: Sum => node
    case _         => Sum(node)
  }

  object keys extends ReducingBarrierStep.Properties
  override lazy val properties: List[Property] = ReducingBarrierStep.properties
  trait Properties extends ReducingBarrierStep.Properties

  def apply(): Sum = {
    val node = DetachedGraph.nodes.create(ontology)

    Sum(node)
  }

}

case class Sum private (override val value: Node) extends WrappedNode(value) with ReducingBarrierStep {
  override def prettyPrint: String = "sum"
}
