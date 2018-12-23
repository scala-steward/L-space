package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Id
    extends StepDef("Id",
                    "An id-step returns the id from the resource held by the traverser.",
                    () => MoveStep.ontology :: Nil)
    with StepWrapper[Id] {

  def wrap(node: Node): Id = node match {
    case node: Id => node
    case _        => new Id(node)
  }

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  def apply(): Id = {
    val node = DetachedGraph.nodes.create(ontology)

    new Id(node)
  }

}

case class Id private (override val value: Node) extends WrappedNode(value) with MoveStep {
  override def prettyPrint: String = "id"
}
