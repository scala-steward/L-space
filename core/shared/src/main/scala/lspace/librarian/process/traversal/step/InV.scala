package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

case object InV
    extends StepDef("InV", "An inV-step ..", () => MoveStep.ontology :: Nil)
    with StepWrapper[InV]
    with InV {

  def toStep(node: Node): InV = this

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  lazy val toNode: Node = DetachedGraph.nodes.create(ontology)

  override def prettyPrint: String = "inV"
}

trait InV extends MoveStep
