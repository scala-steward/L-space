package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.datatype.DataType
import lspace.structure._

case object Head
    extends StepDef("Head", "A head-step limits the traversal to first result.", () => ClipStep.ontology :: Nil)
    with StepWrapper[Head]
    with Head {

  def toStep(node: Node): Head = this

  object keys extends ClipStep.Properties
  override lazy val properties: List[Property] = ClipStep.properties
  trait Properties extends ClipStep.Properties

  implicit def toNode(head: Head): Node = DetachedGraph.nodes.create(ontology)

}

trait Head extends ClipStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "head"
}
