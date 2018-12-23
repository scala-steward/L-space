package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object OutMap extends StepDef("OutMap", "An outMap-step ..", () => MoveStep.ontology :: Nil) with StepWrapper[OutMap] {

  def wrap(node: Node): OutMap = node match {
    case node: OutMap => node
    case _ =>
      new OutMap(
        node
          .outE(MoveStep.keys.label)
//                   .flatMap(_.inV.hasLabel(Property.ontology).map(_.value))
          .map(_.iri)
          .map(iri => node.graph.ns.getProperty(iri).getOrElse(Property(iri))) //TODO: get from target graph(s) or download if not found?
          .toSet,
        node
      )
  }

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  def apply(labels: Set[Property] = Set()): OutMap = {
    val node = DetachedGraph.nodes.create(ontology)

    labels.foreach(label => node.addOut(MoveStep.keys.label, label))
    new OutMap(labels, node)
  }

}

case class OutMap private (label: Set[Property], override val value: Node) extends WrappedNode(value) with MapStep {
  override def prettyPrint: String = "outMap(" + label.map(_.iri).mkString(", ") + ")"
}
