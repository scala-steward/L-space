package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object InEMap extends StepDef("InEMap", "An inEMap-step ..", () => MoveStep.ontology :: Nil) with StepWrapper[InEMap] {

  def wrap(node: Node): InEMap = node match {
    case node: InEMap => node
    case _ =>
      new InEMap(
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

  def apply(labels: Set[Property] = Set()): InEMap = {
    val node = DetachedGraph.nodes.create(ontology)

    labels.foreach(label => node.addOut(keys.`ns.l-space.eu/librarian/MoveStep/label`, label))
    new InEMap(labels, node)
  }

}

case class InEMap private (label: Set[Property], override val value: Node) extends WrappedNode(value) with MapStep {
  override def prettyPrint: String = "inEMap(" + label.map(_.iri).mkString(", ") + ")"
}
