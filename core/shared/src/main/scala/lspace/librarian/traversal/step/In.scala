package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._

object In
    extends StepDef("In",
                    "An in-step takes one or more property-labels and traverses along the valid incoming paths if any",
                    () => MoveStep.ontology :: Nil)
    with StepWrapper[In] {

  def toStep(node: Node): In = In(
    node
      .out(MoveStep.keys.labelUrl)
      .map(_.iri)
      .map(iri => node.graph.ns.properties.cached(iri).getOrElse(Property(iri))) //TODO: get from target graph(s) or download if not found?
      .toSet
  )

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  implicit def toNode(in: In): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    in.label.foreach(label => node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, label))
    node
  }
}

case class In(label: Set[Property]) extends MoveStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "in(" + label.map(_.iri).mkString(", ") + ")"
}
