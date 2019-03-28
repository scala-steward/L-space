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

  def toStep(node: Node): Last = this

  object keys extends ClipStep.Properties
  override lazy val properties: List[Property] = ClipStep.properties
  trait Properties extends ClipStep.Properties

  implicit def toNode(last: Last): Task[Node] = DetachedGraph.nodes.create(ontology)

}

trait Last extends ClipStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "last"
}
