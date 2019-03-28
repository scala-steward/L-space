package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

case object To
    extends StepDef("To", "An to-step moves to the destination of an edge", () => MoveStep.ontology :: Nil)
    with StepWrapper[To]
    with To {

  def toStep(node: Node): To = this

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  lazy val toNode: Task[Node] = DetachedGraph.nodes.create(ontology)

  override def prettyPrint: String = "to"
}

trait To extends MoveStep
