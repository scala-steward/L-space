package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task
import shapeless.{HList, HNil}

object Where extends StepDef("Where", "A where-step ..", FilterStep.ontology :: Nil) with StepWrapper[Where] {

  def toStep(node: Node): Task[Where] =
    for {
      traversal <- node
        .out(keys.traversalTraversal)
        .take(1)
        .map(
          Traversal
            .toTraversal(_)
            .map(_.asInstanceOf[Traversal[_ <: ClassType[_], _ <: ClassType[_], HList]]))
        .head
    } yield Where(traversal)

  object keys extends FilterStep.Properties {
    object traversal
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Where/traversal",
          "traversal",
          "A traversal which must have a non-empty result",
          container = lspace.NS.types.`@list` :: Nil,
          `@range` = Traversal.ontology :: Nil
        )
    val traversalTraversal: TypedProperty[Node] = traversal.property as Traversal.ontology
  }
  override lazy val properties: List[Property] = keys.traversal :: FilterStep.properties
  trait Properties extends FilterStep.Properties {
    val traversal          = keys.traversal
    val traversalTraversal = keys.traversalTraversal
  }

  implicit def toNode(step: Where): Task[Node] = {
    for {
      node      <- DetachedGraph.nodes.create(ontology)
      traversal <- step.traversal.toNode
      _         <- node.addOut(keys.traversalTraversal, traversal)
    } yield node
  }.memoizeOnSuccess
}

case class Where(traversal: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]) extends FilterStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "where(_." + traversal.toString + ")"
}
