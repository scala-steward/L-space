package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import lspace.NS.types
import lspace.datatype.ListType
import monix.eval.Task
import shapeless.{HList, HNil}

object Union
    extends StepDef("Union", "A union-step ..", () => BranchStep.ontology :: Nil)
    with StepWrapper[Union[ClassType[Any], ClassType[Any]]] {

  def toStep(node: Node): Task[Union[ClassType[Any], ClassType[Any]]] =
    for {
      traversals <- Task.gather(
        node
          .out(keys.traversalTraversal)
          .map(
            _.map(Traversal
              .toTraversal(_)
              .map(_.asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])))
          .head)
    } yield Union(traversals)

  object keys extends BranchStep.Properties {
    object traversal
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Union/traversal",
          "traversal",
          "A traversal ..",
          container = lspace.NS.types.`@list` :: Nil,
          `@range` = () => Traversal.ontology :: Nil
        )
    val traversalTraversal: TypedProperty[List[Node]] = traversal.property as ListType(Traversal.ontology)
  }
  override lazy val properties: List[Property] = keys.traversal :: BranchStep.properties
  trait Properties extends BranchStep.Properties {
    val traversal          = keys.traversal
    val traversalTraversal = keys.traversalTraversal
  }

  implicit def toNode(step: Union[_ <: ClassType[_], _ <: ClassType[_]]): Task[Node] = {
    for {
      node       <- DetachedGraph.nodes.create(ontology)
      traversals <- Task.gather(step.traversals.map(_.toNode))
      _          <- node.addOut(keys.traversalTraversal, traversals)
    } yield node
  }.memoizeOnSuccess
}

case class Union[S <: ClassType[_], E <: ClassType[_]](traversals: List[Traversal[S, E, _ <: HList]])
    extends BranchStep {

  lazy val toNode: Task[Node] = this
  override def prettyPrint: String =
    "union(" + traversals.map(_.toString).map("_." + _).mkString(", ") + ")"
}
