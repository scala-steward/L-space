package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.datatype.DataType
import lspace.structure._
import monix.eval.Task

case object Last
    extends StepDef("Last", "A last-step limits the traversal to last result.", () => ClipStep.ontology :: Nil)
    with StepWrapper[Last]
    with Last {

  def toStep(node: Node): Task[Last] = Task.now(this)

  object keys extends ClipStep.Properties
  override lazy val properties: List[Property] = ClipStep.properties
  trait Properties extends ClipStep.Properties

  implicit lazy val toNode: Task[Node] = DetachedGraph.nodes.create(ontology).memoizeOnSuccess
}

trait Last extends ClipStep {

  override def prettyPrint: String = "last"
}
