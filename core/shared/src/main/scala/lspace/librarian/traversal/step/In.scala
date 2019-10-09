package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object In
    extends StepDef("In",
                    "An in-step takes one or more property-labels and traverses along the valid incoming paths if any",
                    MoveStep.ontology :: Nil)
    with StepWrapper[In] {

  def toStep(node: Node): Task[In] =
    for {
      properties <- Task.gatherUnordered(
        node
          .outE(MoveStep.keys.label)
          .map(_.to.iri)
          .filter(_.nonEmpty)
          .map(iri => node.graph.ns.properties.get(iri).map(_.getOrElse(Property(iri))))) //TODO: get from target graph(s) or download if not found?
      out = In(properties.toSet)
    } yield out

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  implicit def toNode(step: In): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- Task.gather(step.label.map(label => node.addOut(MoveStep.keys.label, label)))
    } yield node
  }.memoizeOnSuccess
}

case class In(label: Set[Property]) extends MoveStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "in(" + label.map(_.iri).mkString(", ") + ")"
}
