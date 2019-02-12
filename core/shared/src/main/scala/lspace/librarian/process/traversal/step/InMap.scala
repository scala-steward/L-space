package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

object InMap extends StepDef("InMap", "An inMap-step ..", () => MoveStep.ontology :: Nil) with StepWrapper[InMap] {

  def toStep(node: Node): InMap = InMap(
    node
      .out(MoveStep.keys.labelUrl)
      .map(_.iri)
      .map(iri => node.graph.ns.properties.cached(iri).getOrElse(Property(iri))) //TODO: get from target graph(s) or download if not found?
      .toSet
  )

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  implicit def toNode(inMap: InMap): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    inMap.label.foreach(label => node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, label))
    node
  }
}

case class InMap(label: Set[Property]) extends MapStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "inMap(" + label.map(_.iri).mkString(", ") + ")"
}
