package lspace.librarian.process.traversal.step

import lspace.NS.types
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Is extends StepDef("Is", "An is-step ..", () => FilterStep.ontology :: Nil) with StepWrapper[Is] {

  def toStep(node: Node): Is = Is(node.out(Is.keys.predicateUrl).map(P.toNode))

  object keys {
    object predicate
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Is/Predicate",
          "Predicate",
          "A Predicate",
          container = types.`@list` :: Nil,
          `@range` = () => P.ontology :: Nil
        )
    val predicateUrl: TypedProperty[Node] = predicate.property + P.ontology
  }
  override lazy val properties: List[Property] = keys.predicate :: FilterStep.properties
  trait Properties extends FilterStep.Properties {
    val predicate    = keys.predicate
    val predicateUrl = keys.predicateUrl
  }

  implicit def toNode(is: Is): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    is.predicate.map(_.toNode).foreach(node.addOut(keys.predicate, _))
    node
  }

}

case class Is(predicate: List[P[_]]) extends FilterStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "is(P." + predicate.head.prettyPrint + ")"
}
