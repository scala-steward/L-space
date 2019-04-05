package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

case object Count
    extends StepDef("Count",
                    "A count-step counts the number of remaining traversers.",
                    () => ReducingBarrierStep.ontology :: Nil)
    with StepWrapper[Count]
    with Count {

  def toStep(node: Node): Task[Count] = Task.now(this)

  object keys extends ReducingBarrierStep.Properties
  override lazy val properties: List[Property] = ReducingBarrierStep.properties
  trait Properties extends ReducingBarrierStep.Properties

  lazy val toNode: Task[Node]      = DetachedGraph.nodes.create(ontology).memoizeOnSuccess
  override def prettyPrint: String = "count()"
}

trait Count extends ReducingBarrierStep
