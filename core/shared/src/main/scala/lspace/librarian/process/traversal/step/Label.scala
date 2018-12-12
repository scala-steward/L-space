package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Label extends StepDef("Label") with StepWrapper[Label] {

  def wrap(node: Node): Label = node match {
    case node: Label => node
    case _ =>
      new Label(
        node
          .out(MoveStep.keys.labelUrl)
          .map(_.iri)
          .flatMap(node.graph.ns.getClassType(_)) //TODO:         .getOrElse(throw new Exception("Label with unknown/uncached ontology"))
          .toSet,
        node
      )
  }

  object keys extends MoveStep.Properties

  def apply[CT <: ClassType[_]](labels: Set[CT] = Set()): Label = {
    val node = DetachedGraph.nodes.create(ontology)

    labels.foreach {
      case ontology: Ontology =>
        node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, ontology.asInstanceOf[Ontology])
      case property: Property =>
        node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, property.asInstanceOf[Property])
      case classtype =>
        node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, classtype)
    }
    new Label(labels.asInstanceOf[Set[ClassType[_]]], node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.`ns.l-space.eu/librarian/MoveStep/label`
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Label private (label: Set[ClassType[_]], override val value: Node) extends WrappedNode(value) with MoveStep {
  override def prettyPrint: String = "label(" + label.map(_.iri).mkString(", ") + ")"
}
