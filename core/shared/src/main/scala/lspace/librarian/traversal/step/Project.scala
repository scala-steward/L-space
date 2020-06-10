package lspace.librarian.traversal.step

import lspace.datatype.ListType
import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task
import shapeless.{HList, HNil}

object Project
    extends StepDef("Project", "A project-step ..", ProjectionStep.ontology :: Nil)
    with StepWrapper[Project[HList]] {

  def toStep(node: Node): Task[Project[HList]] =
    for {
      by <- Task.parSequence(
        node
          .out(keys.byTraversal)
          .map(
            _.map(Traversal
              .toTraversal(_)
              .map(_.asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])))
          .head)
    } yield Project[HList](by.reverse.foldLeft[HList](HNil) { case (hlist, traversal) => traversal :: hlist })

  object keys extends ProjectionStep.Properties {
    object by
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Project/by",
          "by",
          "A traversal ..",
          container = lspace.NS.types.`@list` :: Nil,
          `@range` = Traversal.ontology :: Nil
        )
    val byTraversal: TypedProperty[List[Node]] = by.property.as(ListType(Traversal.ontology))
  }
  override lazy val properties: List[Property] = keys.by :: ProjectionStep.properties
  trait Properties extends ProjectionStep.Properties

  implicit def toNode[Traversals <: HList](project: Project[Traversals])
//                                          (
//      implicit
//      lub: LUBConstraint[Traversals, Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]])
    : Task[Node] = {

    for {
      node <- DetachedGraph.nodes.create(ontology)
      traversals <- Task.parSequence(
        project.by.runtimeList
          .map(_.asInstanceOf[Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]])
          .map(_.toNode))
      _ <- node.addOut(keys.byTraversal, traversals)
    } yield node
  }.memoizeOnSuccess

}

case class Project[Traversals <: HList](by: Traversals) extends ProjectionStep {

  lazy val toNode: Task[Node] = this
  override def prettyPrint: String =
    by.runtimeList.reverse.map(_.asInstanceOf[Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]]) match {
      case Nil => "[error:invalid projection]"
      case head :: tail =>
        (head.stepsList match {
          case Nil => "project()"
          case _   => "project(_." + head.prettyPrint + ")"
        }) + (if (tail.nonEmpty) ".by(_." + tail.map(_.prettyPrint).mkString(").by(_.") + ")" else "")
    }
}
