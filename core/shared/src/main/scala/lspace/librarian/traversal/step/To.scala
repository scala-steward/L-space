package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

case object To
    extends StepDef("To", "An to-step moves to the destination of an edge", TraverseStep.ontology :: Nil)
    with StepWrapper[To]
    with To {

  def toStep(node: Node): Task[To] = Task.now(this)

  object keys extends TraverseStep.Properties
  override lazy val properties: List[Property] = TraverseStep.properties
  trait Properties extends TraverseStep.Properties

  lazy val toNode: Task[Node] = DetachedGraph.nodes.create(ontology).memoizeOnSuccess

  override def prettyPrint: String = "to"
}

trait To extends TraverseStep
