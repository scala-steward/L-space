package lspace.librarian.traversal.step

import lspace.NS.types
import lspace.datatype.ListType
import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task
import shapeless.HList

object Coalesce
    extends StepDef(
      "Coalesce",
      "A coalesce-steps continues on the first of n-traversals which has a non-empty result.",
      BranchStep.ontology :: Nil
    )
    with StepWrapper[Coalesce[_, _]] {

  def toStep(node: Node): Task[Coalesce[ClassType[Any], ClassType[Any]]] = node match {
    //    case node: Union[Any, Any, F] => node
    case _ =>
      for {
        traversals <- Task.parSequence(
          node
            .out(keys.traversalTraversal)
            .map(
              _.map(
                Traversal
                  .toTraversal(_)
                  .map(_.asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
              )
            )
            .head
        )
      } yield Coalesce[ClassType[Any], ClassType[Any]](traversals)
  }

  object keys extends BranchStep.Properties {
    object traversal
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Coalesce/traversal",
          "traversal",
          "A traversal ..",
          container = types.`@list` :: Nil,
          `@range` = Traversal.ontology :: Nil
        )
    val traversalTraversal: TypedProperty[List[Node]] = traversal.property.as(ListType(Traversal.ontology))
  }
  override lazy val properties: List[Property] = keys.traversal :: BranchStep.properties

  trait Properties extends BranchStep.Properties {
    lazy val `ns.l-space.eu/librarian/step/Coalesce/traversal`: Property = Coalesce.keys.traversal
    lazy val `ns.l-space.eu/librarian/step/Coalesce/traversal @Traversal`: TypedKey[List[Node]] =
      Coalesce.keys.traversalTraversal
  }

  implicit def toNode(step: Coalesce[_ <: ClassType[_], _ <: ClassType[_]]): Task[Node] = {
    for {
      node       <- DetachedGraph.nodes.create(ontology)
      traversals <- Task.parSequence(step.traversals.map(_.toNode))
      _          <- node.addOut(keys.traversalTraversal, traversals)
    } yield node
  }.memoizeOnSuccess
}

//TODO: traversals needs to be an HList for OutTweaker calculations
case class Coalesce[S <: ClassType[_], E <: ClassType[_]](traversals: List[Traversal[S, E, _ <: HList]])
    extends BranchStep {

  lazy val toNode: Task[Node] = this
  override def prettyPrint: String =
    "coalesce(" + traversals.map(_.toString).map("_." + _).mkString(", ") + ")"
}
