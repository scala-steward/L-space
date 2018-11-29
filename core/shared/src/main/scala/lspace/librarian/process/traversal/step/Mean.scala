package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Mean extends StepDef("Mean") with StepWrapper[Mean] {

  def wrap(node: Node): Mean = node match {
    case node: Mean => node
    case _          => Mean(node)
  }

  def apply(): Mean = {
    val node = DetachedGraph.nodes.create(ontology)

    Mean(node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Mean private (override val value: Node) extends WrappedNode(value) with ReducingBarrierStep {
  override def prettyPrint: String = "mean"
}
