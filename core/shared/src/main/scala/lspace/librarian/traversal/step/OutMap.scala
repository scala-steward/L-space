package lspace.librarian.traversal.step

import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object OutMap extends StepDef("OutMap", "An outMap-step ..", MoveStep.ontology :: Nil) with StepWrapper[OutMap] {

  def toStep(node: Node): Task[OutMap] =
    for {
      properties <- Task.parSequenceUnordered(
        node
          .outE(MoveStep.keys.label)
          .map(_.to.iri)
          .filter(_.nonEmpty)
          .map(iri => node.graph.ns.properties.get(iri).map(_.getOrElse(Property(iri))))
      ) // TODO: get from target graph(s) or download if not found?
      out = OutMap(properties.toSet)
    } yield out

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  implicit def toNode(step: OutMap): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- Task.parSequence(step.label.map(label => node.addOut(MoveStep.keys.label, label)))
    } yield node
  }.memoizeOnSuccess

}

case class OutMap(label: Set[Property]) extends MapStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "outMap(" + label.map(_.iri).mkString(", ") + ")"
}
