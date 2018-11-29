package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Dedup extends StepDef("Dedup") with StepWrapper[Dedup] {

  def wrap(node: Node): Dedup = node match {
    case node: Dedup => node
    case _           => Dedup(node)
  }

  def apply(): Dedup = {
    val node = DetachedGraph.nodes.create(ontology)

    Dedup(node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Dedup private (override val value: Node) extends WrappedNode(value) with Step {
  override def prettyPrint: String = "dedup()"
}
