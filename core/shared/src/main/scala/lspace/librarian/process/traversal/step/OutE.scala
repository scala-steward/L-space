package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object OutE extends StepDef("OutE") with StepWrapper[OutE] {

  def wrap(node: Node): OutE = node match {
    case node: OutE => node
    case _ =>
      new OutE(node
                 .out(MoveStep.keys.labelUrl)
                 .map(Property.apply)
                 .toSet,
               node)
  }

  object keys extends MoveStep.Properties

  def apply(labels: Set[Property] = Set()): OutE = {
    val node = DetachedGraph.createNode(ontology)

    labels.foreach(label => node.addOut(MoveStep.keys.label, label))
    new OutE(labels, node)
  }

  ontologyNode --- Property.default.properties --> keys.`ns.l-space.eu/librarian/MoveStep/label`
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class OutE private (label: Set[Property], override val value: Node) extends WrappedNode(value) with MoveStep {
  override def prettyPrint: String = "outE(" + label.map(_.iri).mkString(", ") + ")"
}
