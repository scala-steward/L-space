package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Out extends StepDef("Out") with StepWrapper[Out] {

  def wrap(node: Node): Out = node match {
    case node: Out => node
    case _ =>
      new Out(node
                .out(MoveStep.keys.labelUrl)
                .map(Property.apply)
                .toSet,
              node)
  }

  object keys extends MoveStep.Properties

  def apply(labels: Set[Property] = Set()): Out = {
    val node = DetachedGraph.nodes.create(ontology)

    labels.foreach(label => node.addOut(MoveStep.keys.label, label))
    new Out(labels, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.`ns.l-space.eu/librarian/MoveStep/label`
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Out private (label: Set[Property], override val value: Node) extends WrappedNode(value) with MoveStep {
  override def prettyPrint: String = "out(" + label.map(_.iri).mkString(", ") + ")"
}
