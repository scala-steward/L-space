package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Min
    extends StepDef(
      "Min",
      "A min-step finds the traverser with the resource with the smallest value within all traversers in-scope.",
      () => ReducingBarrierStep.ontology :: Nil
    )
    with StepWrapper[Min] {

  def wrap(node: Node): Min = node match {
    case node: Min => node
    case _         => Min(node)
  }

  object keys extends ReducingBarrierStep.Properties
  override lazy val properties: List[Property] = ReducingBarrierStep.properties
  trait Properties extends ReducingBarrierStep.Properties

  def apply(): Min = {
    val node = DetachedGraph.nodes.create(ontology)

    Min(node)
  }

}

case class Min private (override val value: Node) extends WrappedNode(value) with ReducingBarrierStep {
  override def prettyPrint: String = "min"
}
