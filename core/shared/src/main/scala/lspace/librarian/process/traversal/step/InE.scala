package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object InE extends StepDef("InE") with StepWrapper[InE] {

  def wrap(node: Node): InE = node match {
    case node: InE => node
    case _ =>
      new InE(node
                .out(MoveStep.keys.labelUrl)
                .map(Property.apply)
                .toSet,
              node)
  }

  object keys extends MoveStep.Properties

  def apply(labels: Set[Property] = Set()): InE = {
    val node = DetachedGraph.createNode(ontology)

    labels.foreach(label => node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, label))
    new InE(labels, node)
  }

  ontologyNode --- Property.default.properties --> MoveStep.keys.label
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class InE private (label: Set[Property], override val value: Node) extends WrappedNode(value) with MoveStep {
  override def prettyPrint: String = "inE(" + label.map(_.iri).mkString(", ") + ")"
}
