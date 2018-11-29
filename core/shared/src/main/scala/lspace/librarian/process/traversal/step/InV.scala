package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object InV extends StepDef("InV") with StepWrapper[InV] {

  def wrap(node: Node): InV = node match {
    case node: InV => node
    case _         => InV(node)
  }

  def apply(): InV = {
    val node = DetachedGraph.nodes.create(ontology)

    InV(node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class InV private (override val value: Node) extends WrappedNode(value) with MoveStep {
  override def prettyPrint: String = "inV"
}
