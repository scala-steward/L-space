package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

case object OutR
    extends StepDef("OutR", "An outR-step ..", () => MoveStep.ontology :: Nil)
    with StepWrapper[MoveStep]
    with MoveStep {

  def toStep(node: Node): this.type = this

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  lazy val toNode: Node            = DetachedGraph.nodes.create(ontology)
  override def prettyPrint: String = "outR"
}
