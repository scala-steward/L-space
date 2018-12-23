package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Dedup
    extends StepDef("Dedup", "A dedup-step deduplicates traversers holding the same resource or result.")
    with StepWrapper[Dedup] {

  def wrap(node: Node): Dedup = node match {
    case node: Dedup => node
    case _           => Dedup(node)
  }

  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties

  def apply(): Dedup = {
    val node = DetachedGraph.nodes.create(ontology)

    Dedup(node)
  }

}

case class Dedup private (override val value: Node) extends WrappedNode(value) with Step {
  override def prettyPrint: String = "dedup()"
}
