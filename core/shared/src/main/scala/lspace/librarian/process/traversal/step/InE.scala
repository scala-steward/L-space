package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object InE
    extends StepDef("InE",
                    "An inE-step takes one or more property-labels and traverses to the valid incoming paths if any",
                    () => MoveStep.ontology :: Nil)
    with StepWrapper[InE] {

  def wrap(node: Node): InE = node match {
    case node: InE => node
    case _ =>
      new InE(
        node
          .out(MoveStep.keys.labelUrl)
          .map(_.iri)
          .map(iri => node.graph.ns.getProperty(iri).getOrElse(Property(iri))) //TODO: get from target graph(s) or download if not found?
          .toSet,
        node
      )
  }

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  def apply(labels: Set[Property] = Set()): InE = {
    val node = DetachedGraph.nodes.create(ontology)

    labels.foreach(label => node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, label))
    new InE(labels, node)
  }

}

case class InE private (label: Set[Property], override val value: Node) extends WrappedNode(value) with MoveStep {
  override def prettyPrint: String = "inE(" + label.map(_.iri).mkString(", ") + ")"
}
