package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object OutR extends StepDef("OutR", "An outR-step ..", () => MoveStep.ontology :: Nil) with StepWrapper[OutR] {

  def wrap(node: Node): OutR = node match {
    case node: OutR => node
    case _          => OutR(node)
  }

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  def apply(): OutR = {
    val node = DetachedGraph.nodes.create(ontology)

    OutR(node)
  }

}

case class OutR private (override val value: Node) extends WrappedNode(value) with MoveStep {
  override def prettyPrint: String = "outR"
}
