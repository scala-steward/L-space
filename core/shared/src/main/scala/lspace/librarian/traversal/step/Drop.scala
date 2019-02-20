package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._

case object Drop
    extends StepDef("Drop", "A drop-step removes all resources, held by the traverers, from the graph.")
    with StepWrapper[Drop]
    with Drop {

  def toStep(node: Node): Drop = this

  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties

  def toNode: Node                 = DetachedGraph.nodes.create(ontology)
  override def prettyPrint: String = "drop()"
}

trait Drop extends Step
