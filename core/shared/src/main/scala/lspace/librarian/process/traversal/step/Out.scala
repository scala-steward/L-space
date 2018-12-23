package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Out
    extends StepDef("Out",
                    "An out-step takes one or more property-labels and traverses along the valid outgoing paths if any",
                    () => MoveStep.ontology :: Nil)
    with StepWrapper[Out] {

  def wrap(node: Node): Out = node match {
    case node: Out => node
    case _ =>
      new Out(
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

  def apply(labels: Set[Property] = Set()): Out = {
    val node = DetachedGraph.nodes.create(ontology)

    labels.foreach(label => node.addOut(MoveStep.keys.label, label))
    new Out(labels, node)
  }

}

case class Out private (label: Set[Property], override val value: Node) extends WrappedNode(value) with MoveStep {
  override def prettyPrint: String = "out(" + label.map(_.iri).mkString(", ") + ")"
}
