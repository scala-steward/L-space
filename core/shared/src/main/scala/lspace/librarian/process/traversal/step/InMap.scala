package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object InMap extends StepDef("InMap") with StepWrapper[InMap] {

  def wrap(node: Node): InMap = node match {
    case node: InMap => node
    case _ =>
      new InMap(node
                  .out(MoveStep.keys.labelUrl)
                  .map(Property.apply)
                  .toSet,
                node)
  }

  object keys extends MoveStep.Properties

  def apply(labels: Set[Property] = Set()): InMap = {
    val node = DetachedGraph.nodes.create(ontology)

    labels.foreach(label => node.addOut(MoveStep.keys.label, label))
    new InMap(labels, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.`ns.l-space.eu/librarian/MoveStep/label`
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class InMap private (label: Set[Property], override val value: Node) extends WrappedNode(value) with MapStep {
  override def prettyPrint: String = "inMap(" + label.map(_.iri).mkString(", ") + ")"
}
