package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Min extends StepDef("Min") with StepWrapper[Min] {

  def wrap(node: Node): Min = node match {
    case node: Min => node
    case _         => Min(node)
  }

  def apply(): Min = {
    val node = DetachedGraph.nodes.create(ontology)

    Min(node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Min private (override val value: Node) extends WrappedNode(value) with ReducingBarrierStep {
  override def prettyPrint: String = "min"
}
