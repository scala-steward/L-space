package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

case object OutV
    extends StepDef("OutV", "An outV-step ..", () => MoveStep.ontology :: Nil)
    with StepWrapper[OutV]
    with OutV {

  def toStep(node: Node): OutV = this

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  lazy val toNode: Node            = DetachedGraph.nodes.create(ontology)
  override def prettyPrint: String = "outV"
}

trait OutV extends MoveStep
