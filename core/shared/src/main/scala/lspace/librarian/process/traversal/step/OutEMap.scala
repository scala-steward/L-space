package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object OutEMap
    extends StepDef("OutEMap", "An outEMap-step ..", () => MoveStep.ontology :: Nil)
    with StepWrapper[OutEMap] {

  def wrap(node: Node): OutEMap = node match {
    case node: OutEMap => node
    case _ =>
      new OutEMap(
        node
          .out(MoveStep.keys.labelUrl)
          .map(_.iri)
          .map(iri => node.graph.ns.getProperty(iri).getOrElse(Property(iri))) //TODO: get from target graph(s) or download if not found?
          .toSet,
        node
      )
  }

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  def apply(labels: Set[Property] = Set()): OutEMap = {
    val node = DetachedGraph.nodes.create(ontology)

    labels.foreach(label => node.addOut(MoveStep.keys.label, label))
    new OutEMap(labels, node)
  }

}

case class OutEMap private (label: Set[Property], override val value: Node) extends WrappedNode(value) with MapStep {
  override def prettyPrint: String = "outEMap(" + label.map(_.iri).mkString(", ") + ")"
}
