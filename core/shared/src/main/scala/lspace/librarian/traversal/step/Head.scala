package lspace.librarian.traversal.step

import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

case object Head
    extends StepDef("Head", "A head-step limits the traversal to first result.", ReducingStep.ontology :: Nil)
    with StepWrapper[Head]
    with Head {

  def toStep(node: Node): Task[Head] = Task.now(this)

  object keys extends ReducingStep.Properties
  override lazy val properties: List[Property] = ReducingStep.properties
  trait Properties extends ReducingStep.Properties

  implicit def toNode(head: Head): Task[Node] = DetachedGraph.nodes.create(ontology).memoizeOnSuccess

}

trait Head extends ReducingStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "head"
}
