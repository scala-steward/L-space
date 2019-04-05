package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.provider.wrapped.WrappedNode
import lspace.structure._
import lspace.NS.types
import lspace.datatype.ListType
import monix.eval.Task
import shapeless.{HList, HNil}

object Or
    extends StepDef("Or",
                    "An or-step traverser only survives if at least one of the n-traversals has a non-empty result.",
                    () => FilterStep.ontology :: Nil)
    with StepWrapper[Or] {

  def toStep(node: Node): Task[Or] =
    for {
      traversals <- Task.gather(
        node
          .out(keys.traversalTraversal)
          .map(
            _.map(Traversal
              .toTraversal(_)
              .map(_.asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])))
          .head)
    } yield Or(traversals)

  object keys extends FilterStep.Properties {
    object traversal
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Or/traversal",
          "traversal",
          "A traversal ..",
          container = lspace.NS.types.`@list` :: Nil,
          `@range` = () => ListType(Traversal.ontology :: Nil) :: Nil
        )
    val traversalTraversal: TypedProperty[List[Node]] = traversal.property + ListType(Traversal.ontology :: Nil)
  }
  override lazy val properties: List[Property] = keys.traversal :: FilterStep.properties
  trait Properties extends FilterStep.Properties {
    val traversal          = keys.traversal
    val traversalTraversal = keys.traversalTraversal
  }

  implicit def toNode(step: Or): Task[Node] = {
    for {
      node       <- DetachedGraph.nodes.create(ontology)
      traversals <- Task.gather(step.traversals.map(_.toNode))
      _          <- node.addOut(keys.traversalTraversal, traversals)
    } yield node
  }.memoizeOnSuccess

}

case class Or(traversals: List[Traversal[_, _, _ <: HList]]) extends FilterStep {

  lazy val toNode: Task[Node] = this
  override def prettyPrint: String =
    "or(" + traversals.map(_.toString).map("_." + _).mkString(", ") + ")"
}
