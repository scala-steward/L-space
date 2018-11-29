package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object OutEMap extends StepDef("OutEMap") with StepWrapper[OutEMap] {

  def wrap(node: Node): OutEMap = node match {
    case node: OutEMap => node
    case _ =>
      new OutEMap(node
                    .out(MoveStep.keys.labelUrl)
                    .map(Property.apply)
                    .toSet,
                  node)
  }

  object keys extends MoveStep.Properties

  def apply(labels: Set[Property] = Set()): OutEMap = {
    val node = DetachedGraph.nodes.create(ontology)

    labels.foreach(label => node.addOut(MoveStep.keys.label, label))
    new OutEMap(labels, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.`ns.l-space.eu/librarian/MoveStep/label`
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class OutEMap private (label: Set[Property], override val value: Node) extends WrappedNode(value) with MapStep {
  override def prettyPrint: String = "outEMap(" + label.map(_.iri).mkString(", ") + ")"
}
