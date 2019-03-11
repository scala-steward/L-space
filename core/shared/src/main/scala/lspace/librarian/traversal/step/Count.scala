package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._

case object Count
    extends StepDef("Count",
                    "A count-step counts the number of remaining traversers.",
                    () => ReducingBarrierStep.ontology :: Nil)
    with StepWrapper[Count]
    with Count {

  def toStep(node: Node): Count = this

  object keys extends ReducingBarrierStep.Properties
  override lazy val properties: List[Property] = ReducingBarrierStep.properties
  trait Properties extends ReducingBarrierStep.Properties

  def toNode: Node                 = DetachedGraph.nodes.create(ontology)
  override def prettyPrint: String = "count()"
}

trait Count extends ReducingBarrierStep