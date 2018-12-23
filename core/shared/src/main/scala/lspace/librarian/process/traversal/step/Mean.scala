package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Mean
    extends StepDef("Mean",
                    "A mean-step calculates the mean value over all traversers in-scope.",
                    () => ReducingBarrierStep.ontology :: Nil)
    with StepWrapper[Mean] {

  def wrap(node: Node): Mean = node match {
    case node: Mean => node
    case _          => Mean(node)
  }

  object keys extends ReducingBarrierStep.Properties
  override lazy val properties: List[Property] = ReducingBarrierStep.properties
  trait Properties extends ReducingBarrierStep.Properties

  def apply(): Mean = {
    val node = DetachedGraph.nodes.create(ontology)

    Mean(node)
  }

}

case class Mean private (override val value: Node) extends WrappedNode(value) with ReducingBarrierStep {
  override def prettyPrint: String = "mean"
}
