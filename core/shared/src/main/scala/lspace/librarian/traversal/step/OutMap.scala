package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object OutMap extends StepDef("OutMap", "An outMap-step ..", () => MoveStep.ontology :: Nil) with StepWrapper[OutMap] {

  def toStep(node: Node): OutMap = OutMap(
    node
      .outE(MoveStep.keys.label)
//                   .flatMap(_.inV.hasLabel(Property.ontology).map(_.value))
      .map(_.iri)
      .map(iri => node.graph.ns.properties.cached(iri).getOrElse(Property(iri))) //TODO: get from target graph(s) or download if not found?
      .toSet
  )

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  implicit def toNode(step: OutMap): Task[Node] =
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- Task.gather(step.label.map(label => node.addOut(MoveStep.keys.label, label)))
    } yield node

}

case class OutMap(label: Set[Property]) extends MapStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "outMap(" + label.map(_.iri).mkString(", ") + ")"
}
