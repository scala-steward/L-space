package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Drop extends StepDef("Drop") with StepWrapper[Drop] {

  def wrap(node: Node): Drop = node match {
    case node: Drop => node
    case _          => Drop(node)
  }

  def apply(): Drop = {
    val node = DetachedGraph.createNode(ontology)

    Drop(node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Drop private (override val value: Node) extends WrappedNode(value) with Step {
  override def prettyPrint: String = "drop()"
}
