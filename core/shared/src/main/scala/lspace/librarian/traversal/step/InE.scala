package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.provider.wrapped.WrappedNode
import lspace.structure._

object InE
    extends StepDef("InE",
                    "An inE-step takes one or more property-labels and traverses to the valid incoming paths if any",
                    () => MoveStep.ontology :: Nil)
    with StepWrapper[InE] {

  def toStep(node: Node): InE = InE(
    node
      .out(MoveStep.keys.labelUrl)
      .map(_.iri)
      .map(iri => node.graph.ns.properties.cached(iri).getOrElse(Property(iri))) //TODO: get from target graph(s) or download if not found?
      .toSet
  )

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  implicit def toNode(inE: InE): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    inE.label.foreach(label => node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, label))
    node
  }

}

case class InE(label: Set[Property]) extends MoveStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "inE(" + label.map(_.iri).mkString(", ") + ")"
}
