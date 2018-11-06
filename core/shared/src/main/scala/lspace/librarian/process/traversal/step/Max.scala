package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Max extends StepDef("Max") with StepWrapper[Max] {

  def wrap(node: Node): Max = node match {
    case node: Max => node
    case _         => Max(node)
  }

  def apply(): Max = {
    val node = DetachedGraph.createNode(ontology)

    Max(node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Max private (override val value: Node) extends WrappedNode(value) with ReducingBarrierStep {
  override def prettyPrint: String = "max"
}
