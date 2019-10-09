package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object Label extends StepDef("Label", "A label-step ..", MoveStep.ontology :: Nil) with StepWrapper[Label] {

  def toStep(node: Node): Task[Label] =
    for {
      labels <- Task.gather(
        node
          .out(keys.`ns.l-space.eu/librarian/MoveStep/label`)
          .collect {
            case node: Node => node.iri
          }
          .map(node.graph.ns.classtypes.get(_).map(_.get)))
    } yield
      Label(
        labels //TODO:         .getOrElse(throw new Exception("Label with unknown/uncached ontology"))
        .toSet
      )

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  implicit def toNode(step: Label): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _ <- Task.gather(step.label.map {
        case ontology: Ontology =>
          node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, ontology.asInstanceOf[Ontology])
        case property: Property =>
          node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, property.asInstanceOf[Property])
        case classtype =>
          node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, classtype)
      })
    } yield node
  }.memoizeOnSuccess
}

case class Label(label: Set[ClassType[_]] = Set()) extends MoveStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "label(" + label.map(_.iri).mkString(", ") + ")"
}
