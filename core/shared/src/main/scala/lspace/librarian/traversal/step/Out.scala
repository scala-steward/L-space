package lspace.librarian.traversal.step

import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object Out
    extends StepDef("Out",
                    "An out-step takes one or more property-labels and traverses along the valid outgoing paths if any",
                    MoveStep.ontology :: Nil)
    with StepWrapper[Out] {

  def toStep(node: Node): Task[Out] =
    for {
      properties <- Task.parSequenceUnordered(
        node
          .outE(MoveStep.keys.label)
          .map(_.to.iri)
          .filter(_.nonEmpty)
          .map(iri => node.graph.ns.properties.get(iri).map(_.getOrElse(Property(iri))))) //TODO: get from target graph(s) or download if not found?
      out = Out(properties.toSet)
    } yield out

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  implicit def toNode(step: Out): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- Task.parSequence(step.label.map(label => node.addOut(MoveStep.keys.label, label)))
    } yield node
  }.memoizeOnSuccess
}

case class Out(label: Set[Property]) extends MoveStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "out(" + label.map(_.iri).mkString(", ") + ")"
}
