package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

case object Mean
    extends StepDef("Mean",
                    "A mean-step calculates the mean value over all traversers in-scope.",
                    () => ReducingBarrierStep.ontology :: Nil)
    with StepWrapper[Mean]
    with Mean {

  def toStep(node: Node): Mean = this

  object keys extends ReducingBarrierStep.Properties
  override lazy val properties: List[Property] = ReducingBarrierStep.properties
  trait Properties extends ReducingBarrierStep.Properties

  lazy val toNode: Task[Node]      = DetachedGraph.nodes.create(ontology)
  override def prettyPrint: String = "mean"
}

trait Mean extends ReducingBarrierStep
