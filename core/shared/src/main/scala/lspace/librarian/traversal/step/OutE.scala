package lspace.librarian.traversal.step

import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object OutE
    extends StepDef("OutE",
                    "An outE-step takes one or more property-labels and traverses to the valid incoming paths if any",
                    MoveStep.ontology :: Nil)
    with StepWrapper[OutE] {

  def toStep(node: Node): Task[OutE] =
    for {
      properties <- Task.parSequenceUnordered(
        node
          .outE(MoveStep.keys.label)
          .map(_.to.iri)
          .filter(_.nonEmpty)
          .map(iri => node.graph.ns.properties.get(iri).map(_.getOrElse(Property(iri))))) //TODO: get from target graph(s) or download if not found?
      out = OutE(properties.toSet)
    } yield out

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  implicit def toNode(step: OutE): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- Task.parSequence(step.label.map(label => node.addOut(MoveStep.keys.label, label)))
    } yield node
  }.memoizeOnSuccess

}

case class OutE(label: Set[Property]) extends MoveStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "outE(" + label.map(_.iri).mkString(", ") + ")"
}
