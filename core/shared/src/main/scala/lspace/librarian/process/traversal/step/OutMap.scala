package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object OutMap extends StepDef("OutMap") with StepWrapper[OutMap] {

  def wrap(node: Node): OutMap = node match {
    case node: OutMap => node
    case _ =>
      new OutMap(node
                   .outE(MoveStep.keys.label)
                   .flatMap(_.inV.hasLabel(Property.ontology).map(_.value))
                   .map(Property.apply)
                   .toSet,
                 node)
  }

  object keys extends MoveStep.Properties

  def apply(labels: Set[Property] = Set()): OutMap = {
    val node = DetachedGraph.nodes.create(ontology)

    labels.foreach(label => node.addOut(MoveStep.keys.label, label))
    new OutMap(labels, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.`ns.l-space.eu/librarian/MoveStep/label`
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class OutMap private (label: Set[Property], override val value: Node) extends WrappedNode(value) with MapStep {
  override def prettyPrint: String = "outMap(" + label.map(_.iri).mkString(", ") + ")"
}
