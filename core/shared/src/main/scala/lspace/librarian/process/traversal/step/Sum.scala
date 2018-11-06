package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Sum extends StepDef("Sum") with StepWrapper[Sum] {

  def wrap(node: Node): Sum = node match {
    case node: Sum => node
    case _         => Sum(node)
  }

  def apply(): Sum = {
    val node = DetachedGraph.createNode(ontology)

    Sum(node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Sum private (override val value: Node) extends WrappedNode(value) with ReducingBarrierStep {
  override def prettyPrint: String = "sum"
}
