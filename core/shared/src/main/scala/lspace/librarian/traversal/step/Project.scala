package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.provider.wrapped.WrappedNode
import lspace.structure._
import lspace.NS.types
import lspace.datatype.ListType
import monix.eval.Task
import shapeless.{HList, HNil, LUBConstraint}

object Project
    extends StepDef("Project", "A project-step ..", () => TraverseStep.ontology :: Nil)
    with StepWrapper[Project[HList]] {

  def toStep(node: Node): Task[Project[HList]] =
    for {
      by <- Task.gather(
        node
          .out(keys.byTraversal)
          .map(
            _.map(Traversal
              .toTraversal(_)
              .map(_.asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])))
          .head)
    } yield Project[HList](by.reverse.foldLeft[HList](HNil) { case (hlist, traversal) => traversal :: hlist })

  object keys extends TraverseStep.Properties {
    object by
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Project/by",
          "by",
          "A traversal ..",
          container = lspace.NS.types.`@list` :: Nil,
          `@range` = () => Traversal.ontology :: Nil
        )
    val byTraversal: TypedProperty[List[Node]] = by.property + ListType(Traversal.ontology :: Nil)
  }
  override lazy val properties: List[Property] = keys.by :: TraverseStep.properties
  trait Properties extends TraverseStep.Properties

  implicit def toNode[Traversals <: HList](project: Project[Traversals])
//                                          (
//      implicit
//      lub: LUBConstraint[Traversals, Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]])
    : Task[Node] = {

    for {
      node <- DetachedGraph.nodes.create(ontology)
      traversals <- Task.gather(
        project.by.runtimeList
          .map(_.asInstanceOf[Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]])
          .map(_.toNode))
      _ <- node.addOut(keys.byTraversal, traversals)
    } yield node
  }.memoizeOnSuccess

}

case class Project[Traversals <: HList](by: Traversals)
//                                       (
//    implicit
//    lub: LUBConstraint[Traversals, Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]])
    extends TraverseStep {

  lazy val toNode: Task[Node] = this
  override def prettyPrint: String =
    "project(" + by.runtimeList
      .map(_.asInstanceOf[Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]])
      .map(_.prettyPrint)
      .map("_." + _)
      .mkString(", ") + ")"
}
