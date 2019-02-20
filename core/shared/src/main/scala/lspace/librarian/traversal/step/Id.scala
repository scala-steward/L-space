package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._

case object Id
    extends StepDef("Id",
                    "An id-step returns the id from the resource held by the traverser.",
                    () => MoveStep.ontology :: Nil)
    with StepWrapper[Id]
    with Id {

  def toStep(node: Node): Id = this

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  lazy val toNode: Node = DetachedGraph.nodes.create(ontology)

  override def prettyPrint: String = "id"
}

trait Id extends MoveStep
