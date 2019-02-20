package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Label extends StepDef("Label", "A label-step ..", () => MoveStep.ontology :: Nil) with StepWrapper[Label] {

  def toStep(node: Node): Label = Label(
    node
      .out(MoveStep.keys.labelUrl)
      .map(_.iri)
      .flatMap(node.graph.ns.classtypes.cached(_)) //TODO:         .getOrElse(throw new Exception("Label with unknown/uncached ontology"))
      .toSet
  )

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  implicit def toNode(label: Label): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    label.label.foreach {
      case ontology: Ontology =>
        node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, ontology.asInstanceOf[Ontology])
      case property: Property =>
        node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, property.asInstanceOf[Property])
      case classtype =>
        node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, classtype)
    }
    node
  }
}

case class Label(label: Set[ClassType[_]]) extends MoveStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "label(" + label.map(_.iri).mkString(", ") + ")"
}
