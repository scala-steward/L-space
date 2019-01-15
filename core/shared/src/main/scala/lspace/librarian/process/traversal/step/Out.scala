package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Out
    extends StepDef("Out",
                    "An out-step takes one or more property-labels and traverses along the valid outgoing paths if any",
                    () => MoveStep.ontology :: Nil)
    with StepWrapper[Out] {

  def toStep(node: Node): Out = Out(
    node
      .out(MoveStep.keys.labelUrl)
      .map(_.iri)
      .map(iri => node.graph.ns.properties.get(iri).getOrElse(Property(iri))) //TODO: get from target graph(s) or download if not found?
      .toSet
  )

  object keys extends MoveStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties
  trait Properties extends MoveStep.Properties

  implicit def toNode(out: Out): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    out.label.foreach(label => node.addOut(MoveStep.keys.label, label))
    node
  }
}

case class Out(label: Set[Property]) extends MoveStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "out(" + label.map(_.iri).mkString(", ") + ")"
}
