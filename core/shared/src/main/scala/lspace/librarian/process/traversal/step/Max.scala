package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

case object Max
    extends StepDef("Max",
                    "A max-step finds the traverser with the largest value within all traversers in-scope.",
                    () => FilterBarrierStep.ontology :: Nil)
    with StepWrapper[Max]
    with Max {

  def toStep(node: Node): Max = this

  object keys extends ReducingBarrierStep.Properties
  override lazy val properties: List[Property] = ReducingBarrierStep.properties
  trait Properties extends ReducingBarrierStep.Properties

  lazy val toNode: Node            = DetachedGraph.nodes.create(ontology)
  override def prettyPrint: String = "max"
}

trait Max extends FilterBarrierStep
