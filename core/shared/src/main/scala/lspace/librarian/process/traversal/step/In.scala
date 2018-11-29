package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object In extends StepDef("In") with StepWrapper[In] {

  def wrap(node: Node): In = node match {
    case node: In => node
    case _ =>
      new In(node
               .out(MoveStep.keys.labelUrl)
               .map(Property.apply)
               .toSet,
             node)
  }

  object keys extends MoveStep.Properties

  def apply(labels: Set[Property] = Set()): In = {
    val node = DetachedGraph.nodes.create(ontology)

    labels.foreach(label => node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, label))
    new In(labels, node)
  }

  ontologyNode --- Property.default.`@properties` --> MoveStep.keys.label
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class In private (label: Set[Property], override val value: Node) extends WrappedNode(value) with MoveStep {
  override def prettyPrint: String = "in(" + label.map(_.iri).mkString(", ") + ")"
}
