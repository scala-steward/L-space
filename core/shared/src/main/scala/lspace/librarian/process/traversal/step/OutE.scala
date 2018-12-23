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

  def wrap(node: Node): OutE = node match {
    case node: OutE => node
    case _ =>
      new OutE(
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

  def apply(labels: Set[Property] = Set()): OutE = {
    val node = DetachedGraph.nodes.create(ontology)

    labels.foreach(label => node.addOut(MoveStep.keys.label, label))
    new OutE(labels, node)
  }

}

case class OutE private (label: Set[Property], override val value: Node) extends WrappedNode(value) with MoveStep {
  override def prettyPrint: String = "outE(" + label.map(_.iri).mkString(", ") + ")"
}
