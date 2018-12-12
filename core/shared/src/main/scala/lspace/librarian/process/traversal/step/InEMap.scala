package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object InEMap extends StepDef("InEMap") with StepWrapper[InEMap] {

  def wrap(node: Node): InEMap = node match {
    case node: InEMap => node
    case _ =>
      new InEMap(node
                   .out(MoveStep.keys.labelUrl)
                   .map(_.iri)
                   .map(Property.apply)
                   .toSet,
                 node)
  }

  object keys extends MoveStep.Properties

  def apply(labels: Set[Property] = Set()): InEMap = {
    val node = DetachedGraph.nodes.create(ontology)

    labels.foreach(label => node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, label))
    new InEMap(labels, node)
  }

  ontologyNode --- Property.default.`@properties` --> MoveStep.keys.label
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class InEMap private (label: Set[Property], override val value: Node) extends WrappedNode(value) with MapStep {
  override def prettyPrint: String = "inEMap(" + label.map(_.iri).mkString(", ") + ")"
}
