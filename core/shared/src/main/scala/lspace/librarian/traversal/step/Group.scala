package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task
import shapeless.{HList, HNil}

object Group
    extends StepDef("Group", "A group-step groups traversers.", () => CollectingBarrierStep.ontology :: Nil)
    with StepWrapper[Group[ClassType[Any], HList]] {

  def toStep(node: Node): Group[ClassType[Any], HList] =
    Group[ClassType[Any], HList](
      node
        .out(keys.byTraversal)
        .take(1)
        .map(Traversal.toTraversal(_))
        .head)

  object keys extends CollectingBarrierStep.Properties {
    object by
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Group/by",
          "by",
          "A traversal ..",
          `@range` = () => Traversal.ontology :: Nil
        )
    val byTraversal: TypedProperty[Node] = by.property + Traversal.ontology
  }
  override lazy val properties: List[Property] = keys.by :: CollectingBarrierStep.properties

  trait Properties extends CollectingBarrierStep.Properties {
    lazy val `ns.l-space.eu/librarian/step/Group/by`: Property                  = keys.by
    lazy val `ns.l-space.eu/librarian/step/Group/by @Traversal`: TypedKey[Node] = keys.byTraversal
  }

  implicit def toNode[ET <: ClassType[_], Segments <: HList](step: Group[ET, Segments]): Task[Node] = {
    for {
      node      <- DetachedGraph.nodes.create(ontology)
      traversal <- step.by.toNode
      _         <- node.addOut(keys.byTraversal, traversal)
    } yield node
  }

}

case class Group[+ET <: ClassType[_], Segments <: HList](by: Traversal[_ <: ClassType[_], ET, Segments])
    extends CollectingBarrierStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "group(_." + by.toString + ")"
}
