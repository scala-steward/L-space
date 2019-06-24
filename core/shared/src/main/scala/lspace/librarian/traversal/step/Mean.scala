package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

case object Mean
    extends StepDef("Mean",
                    "A mean-step calculates the mean value over all traversers in-scope.",
                    () => ReducingBarrierStep.ontology :: ReducingStep.ontology :: Nil)
    with StepWrapper[Mean]
    with Mean {

  def toStep(node: Node): Task[Mean] = Task.now(this)

  object keys extends ReducingBarrierStep.Properties with ReducingStep.Properties
  override lazy val properties: List[Property] = ReducingBarrierStep.properties ++ ReducingStep.properties
  trait Properties extends ReducingBarrierStep.Properties with ReducingStep.Properties

  implicit lazy val toNode: Task[Node] = DetachedGraph.nodes.create(ontology).memoizeOnSuccess
  override def prettyPrint: String     = "mean"
}

trait Mean extends ReducingBarrierStep with ReducingStep
