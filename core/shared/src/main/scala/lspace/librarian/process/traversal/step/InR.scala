package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal.{MoveStep, Step, StepDef, StepWrapper}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object InR extends StepDef("InR", "An inR-step ..", () => MoveStep.ontology :: Nil) with StepWrapper[InR] {

  def wrap(node: Node): InR = node match {
    case node: InR => node
    case _         => InR(node)
  }

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  def apply(): InR = {
    val node = DetachedGraph.nodes.create(ontology)

    InR(node)
  }

}

case class InR private (override val value: Node) extends WrappedNode(value) with MoveStep {
  override def prettyPrint: String = "inR"
}
