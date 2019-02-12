package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object InEMap extends StepDef("InEMap", "An inEMap-step ..", () => MoveStep.ontology :: Nil) with StepWrapper[InEMap] {

  def toStep(node: Node): InEMap = InEMap(
    node
      .out(MoveStep.keys.labelUrl)
      .map(_.iri)
      .map(iri => node.graph.ns.properties.cached(iri).getOrElse(Property(iri))) //TODO: get from target graph(s) or download if not found?
      .toSet
  )

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  implicit def toNode(inEMap: InEMap): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    inEMap.label.foreach(label => node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, label))
    node
  }
}

case class InEMap(label: Set[Property]) extends MapStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "inEMap(" + label.map(_.iri).mkString(", ") + ")"
}
