package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.datatype.DataType
import lspace.structure._
import monix.eval.Task

case object Head
    extends StepDef("Head", "A head-step limits the traversal to first result.", () => ClipStep.ontology :: Nil)
    with StepWrapper[Head]
    with Head {

  def toStep(node: Node): Task[Head] = Task.now(this)

  object keys extends ClipStep.Properties
  override lazy val properties: List[Property] = ClipStep.properties
  trait Properties extends ClipStep.Properties

  implicit def toNode(head: Head): Task[Node] = DetachedGraph.nodes.create(ontology).memoizeOnSuccess

}

trait Head extends ClipStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "head"
}
