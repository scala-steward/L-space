package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

case object Min
    extends StepDef(
      "Min",
      "A min-step finds the traverser with the resource with the smallest value within all traversers in-scope.",
      () => ReducingBarrierStep.ontology :: Nil
    )
    with StepWrapper[Min]
    with Min {

  def toStep(node: Node): Min = this

  object keys extends ReducingBarrierStep.Properties
  override lazy val properties: List[Property] = ReducingBarrierStep.properties
  trait Properties extends ReducingBarrierStep.Properties

  lazy val toNode: Node            = DetachedGraph.nodes.create(ontology)
  override def prettyPrint: String = "min"
}

trait Min extends ReducingBarrierStep
