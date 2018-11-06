package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Id extends StepDef("Id") with StepWrapper[Id] {

  def wrap(node: Node): Id = node match {
    case node: Id => node
    case _        => new Id(node)
  }

  def apply(): Id = {
    val node = DetachedGraph.createNode(ontology)

    new Id(node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Id private (override val value: Node) extends WrappedNode(value) with MoveStep {
  override def prettyPrint: String = "id"
}
