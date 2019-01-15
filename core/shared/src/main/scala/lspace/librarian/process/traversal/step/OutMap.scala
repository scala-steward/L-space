package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object OutMap extends StepDef("OutMap", "An outMap-step ..", () => MoveStep.ontology :: Nil) with StepWrapper[OutMap] {

  def toStep(node: Node): OutMap = OutMap(
    node
      .outE(MoveStep.keys.label)
//                   .flatMap(_.inV.hasLabel(Property.ontology).map(_.value))
      .map(_.iri)
      .map(iri => node.graph.ns.properties.get(iri).getOrElse(Property(iri))) //TODO: get from target graph(s) or download if not found?
      .toSet
  )

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  implicit def toNode(outMap: OutMap): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    outMap.label.foreach(label => node.addOut(MoveStep.keys.label, label))
    node
  }
}

case class OutMap(label: Set[Property]) extends MapStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "outMap(" + label.map(_.iri).mkString(", ") + ")"
}
