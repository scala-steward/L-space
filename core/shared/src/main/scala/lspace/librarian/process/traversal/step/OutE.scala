package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object OutE
    extends StepDef("OutE",
                    "An outE-step takes one or more property-labels and traverses to the valid incoming paths if any",
                    () => MoveStep.ontology :: Nil)
    with StepWrapper[OutE] {

  def toStep(node: Node): OutE = OutE(
    node
      .out(MoveStep.keys.labelUrl)
      .map(_.iri)
      .map(iri => node.graph.ns.getProperty(iri).getOrElse(Property(iri))) //TODO: get from target graph(s) or download if not found?
      .toSet
  )

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  implicit def toNode(outE: OutE): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    outE.label.foreach(label => node.addOut(MoveStep.keys.label, label))
    node
  }

}

case class OutE(label: Set[Property]) extends MoveStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "outE(" + label.map(_.iri).mkString(", ") + ")"
}
