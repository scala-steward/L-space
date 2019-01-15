package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

case object To
    extends StepDef("To", "An to-step moves to the destination of an edge", () => MoveStep.ontology :: Nil)
    with StepWrapper[To]
    with To {

  def toStep(node: Node): To = this

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  lazy val toNode: Node = DetachedGraph.nodes.create(ontology)

  override def prettyPrint: String = "inV"
}

trait To extends MoveStep
