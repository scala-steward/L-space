package lspace.librarian.traversal.step

import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

case object Sum
    extends StepDef("Sum",
                    "A sum-step calculates the summed value of all traversers in-scope.",
                    ReducingBarrierStep.ontology :: ReducingStep.ontology :: Nil)
    with StepWrapper[Sum]
    with Sum {

  def toStep(node: Node): Task[Sum] = Task.now(this)

  object keys extends ReducingBarrierStep.Properties with ReducingStep.Properties
  override lazy val properties: List[Property] = ReducingBarrierStep.properties ++ ReducingStep.properties
  trait Properties extends ReducingBarrierStep.Properties with ReducingStep.Properties

  lazy val toNode: Task[Node]      = DetachedGraph.nodes.create(ontology).memoizeOnSuccess
  override def prettyPrint: String = "sum"
}

trait Sum extends ReducingBarrierStep with ReducingStep
