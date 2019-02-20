package lspace.librarian.traversal.step

import lspace.NS.types
import lspace.librarian.logic.predicate.P
import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._

object Is extends StepDef("Is", "An is-step ..", () => FilterStep.ontology :: Nil) with StepWrapper[Is] {

  def toStep(node: Node): Is = Is(node.out(Is.keys.predicateUrl).map(P.toP).head)

  object keys {
    object predicate
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Is/Predicate",
          "Predicate",
          "A Predicate",
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
    node.addOut(keys.predicate, is.predicate.toNode)
    node
  }

}

case class Is(predicate: P[_]) extends FilterStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "is(P." + predicate.prettyPrint + ")"
}
