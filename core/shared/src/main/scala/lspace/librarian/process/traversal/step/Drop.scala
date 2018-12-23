package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Drop
    extends StepDef("Drop", "A drop-step removes all resources, held by the traverers, from the graph.")
    with StepWrapper[Drop] {

  def wrap(node: Node): Drop = node match {
    case node: Drop => node
    case _          => Drop(node)
  }

  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties

  def apply(): Drop = {
    val node = DetachedGraph.nodes.create(ontology)

    Drop(node)
  }

}

case class Drop private (override val value: Node) extends WrappedNode(value) with Step {
  override def prettyPrint: String = "drop()"
}
