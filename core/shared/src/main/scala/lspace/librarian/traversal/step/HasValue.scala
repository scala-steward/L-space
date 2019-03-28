package lspace.librarian.traversal.step

import lspace.librarian.logic.predicate.P
import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object HasValue
    extends StepDef("HasValue",
                    "A hasValue-step is successful if the resources satisfies certain predicates.",
                    () => HasStep.ontology :: Nil)
    with StepWrapper[HasValue] {

  def toStep(node: Node): HasValue = HasValue(node.out(Has.keys.predicateUrl).map(P.toP).head)

  object keys {
    val predicate = Has.keys.predicate
  }
  override lazy val properties: List[Property] = keys.predicate :: HasStep.properties
  trait Properties extends HasStep.Properties {
    val predicate = keys.predicate
  }

  implicit def toNode(step: HasValue): Task[Node] = {
    for {
      node      <- DetachedGraph.nodes.create(ontology)
      predicate <- step.predicate.toNode
      _         <- node.addOut(keys.predicate, predicate)
    } yield node
  }
}

case class HasValue(predicate: P[_]) extends HasStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "hasValue(" + predicate.prettyPrint + ")"
}
