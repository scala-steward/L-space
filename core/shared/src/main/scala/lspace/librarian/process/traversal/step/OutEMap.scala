package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object OutEMap
    extends StepDef("OutEMap", "An outEMap-step ..", () => MoveStep.ontology :: Nil)
    with StepWrapper[OutEMap] {

  def toStep(node: Node): OutEMap = OutEMap(
    node
      .out(MoveStep.keys.labelUrl)
      .map(_.iri)
      .map(iri => node.graph.ns.properties.cached(iri).getOrElse(Property(iri))) //TODO: get from target graph(s) or download if not found?
      .toSet
  )

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  implicit def toNode(outEMap: OutEMap): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    outEMap.label.foreach(label => node.addOut(MoveStep.keys.label, label))
    node
  }

}

case class OutEMap(label: Set[Property]) extends MapStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "outEMap(" + label.map(_.iri).mkString(", ") + ")"
}
