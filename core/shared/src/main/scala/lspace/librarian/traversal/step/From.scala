package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

case object From
    extends StepDef("From", "A from-step moves to the origin of the edge", () => MoveStep.ontology :: Nil)
    with StepWrapper[From]
    with From {

  def toStep(node: Node): Task[From] = Task.now(this)

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  lazy val toNode: Task[Node]      = DetachedGraph.nodes.create(ontology).memoizeOnSuccess
  override def prettyPrint: String = "from"
}

trait From extends MoveStep
