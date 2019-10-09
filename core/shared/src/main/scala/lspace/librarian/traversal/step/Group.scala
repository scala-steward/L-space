package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task
import shapeless.{HList, HNil}

object Group
    extends StepDef("Group", "A group-step groups traversers.", GroupingBarrierStep.ontology :: Nil)
    with StepWrapper[Group[ClassType[Any], _ <: HList, ClassType[Any], _ <: HList]] {

  def toStep(node: Node): Task[Group[ClassType[Any], _ <: HList, ClassType[Any], _ <: HList]] =
    for {
      by <- Traversal.toTraversal(
        node
          .out(keys.byTraversal)
          .take(1)
          .head)
      value <- Traversal.toTraversal(
        node
          .out(keys.valueTraversal)
          .take(1)
          .head)
    } yield Group(by, value)

  object keys extends GroupingBarrierStep.Properties {
    object by
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Group/by",
          "by",
          "A traversal ..",
          `@range` = Traversal.ontology :: Nil
        )
    val byTraversal: TypedProperty[Node] = by.property as Traversal.ontology
    object value
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Group/value",
          "value",
          "A traversal ..",
          `@range` = Traversal.ontology :: Nil
        )
    val valueTraversal: TypedProperty[Node] = value.property as Traversal.ontology
  }
  override lazy val properties: List[Property] = keys.by :: GroupingBarrierStep.properties

  trait Properties extends GroupingBarrierStep.Properties {
    lazy val `ns.l-space.eu/librarian/step/Group/by`: Property                  = keys.by
    lazy val `ns.l-space.eu/librarian/step/Group/by @Traversal`: TypedKey[Node] = keys.byTraversal
  }

  implicit def toNode[ET <: ClassType[_], Segments <: HList, ETv <: ClassType[_], SegmentsV <: HList](
      step: Group[ET, Segments, ETv, SegmentsV]): Task[Node] = {
    for {
      node  <- DetachedGraph.nodes.create(ontology)
      by    <- step.by.toNode
      value <- step.value.toNode
      _     <- node.addOut(keys.byTraversal, by)
      _     <- node.addOut(keys.valueTraversal, value)
    } yield node
  }.memoizeOnSuccess

}

case class Group[+ET <: ClassType[Any], Steps <: HList, +ETv <: ClassType[Any], StepsV <: HList](
    by: Traversal[_ <: ClassType[Any], ET, Steps],
    value: Traversal[_ <: ClassType[Any], ETv, StepsV])
    extends GroupingBarrierStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "group(_." + by.toString + ")"
}
