package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object InMap extends StepDef("InMap", "An inMap-step ..", () => MoveStep.ontology :: Nil) with StepWrapper[InMap] {

  def toStep(node: Node): InMap = InMap(
    node
      .out(MoveStep.keys.labelUrl)
      .map(_.iri)
      .map(iri => node.graph.ns.properties.cached(iri).getOrElse(Property(iri))) //TODO: get from target graph(s) or download if not found?
      .toSet
  )

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  implicit def toNode(step: InMap): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- Task.gather(step.label.map(label => node.addOut(MoveStep.keys.label, label)))
    } yield node
  }
}

case class InMap(label: Set[Property]) extends MapStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "inMap(" + label.map(_.iri).mkString(", ") + ")"
}
