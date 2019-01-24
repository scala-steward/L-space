package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._
import shapeless.{HList, HNil}

object Group
    extends StepDef("Group", "A group-step groups traversers.", () => CollectingBarrierStep.ontology :: Nil)
    with StepWrapper[Group[ClassType[Any]]] {

  def toStep(node: Node): Group[ClassType[Any]] =
    Group(
      node
        .out(keys.byTraversal)
        .take(1)
        .map(Traversal.toTraversal(_)(DetachedGraph))
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

  implicit def toNode[A <: ClassType[_]](group: Group[A]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(keys.by, group.by.toNode)
    node
  }

}

case class Group[A <: ClassType[_]](by: Traversal[_ <: ClassType[_], A, _ <: HList]) extends CollectingBarrierStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "group(_." + by.toString + ")"
}
