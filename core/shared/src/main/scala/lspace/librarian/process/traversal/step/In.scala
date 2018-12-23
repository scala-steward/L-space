package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object In
    extends StepDef("In",
                    "An in-step takes one or more property-labels and traverses along the valid incoming paths if any",
                    () => MoveStep.ontology :: Nil)
    with StepWrapper[In] {

  def wrap(node: Node): In = node match {
    case node: In => node
    case _ =>
      new In(
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

  def apply(labels: Set[Property] = Set()): In = {
    val node = DetachedGraph.nodes.create(ontology)

    labels.foreach(label => node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, label))
    new In(labels, node)
  }
}

case class In private (label: Set[Property], override val value: Node) extends WrappedNode(value) with MoveStep {
  override def prettyPrint: String = "in(" + label.map(_.iri).mkString(", ") + ")"
}
