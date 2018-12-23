package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import shapeless.HList

object Repeat extends StepDef("Repeat") with StepWrapper[Repeat[ClassType[Any]]] {

  def wrap(node: Node): Repeat[ClassType[Any]] = node match {
    //    case node: Local[F] => node
    case _ =>
      Repeat(
        node
          .out(keys.traversalTraversal)
          .take(1)
          .map(
            Traversal
              .wrap(_)(DetachedGraph)
              .asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
          .head,
        node
          .out(keys.untilTraversal)
          .take(1)
          .map(
            Traversal
              .wrap(_)(DetachedGraph)
              .asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
          .headOption,
        node.out(keys.maxInt).headOption,
        node.out(keys.collectBoolean).headOption,
        node
      )
  }

  object keys {
    object traversal
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Repeat/traversal",
          "by",
          "A traversal ..",
          `@range` = () => Traversal.ontology :: Nil
        )
    val traversalTraversal: TypedProperty[Node] = traversal.property + Traversal.ontology

    object until
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Repeat/until",
          "until",
          "If the result of this traversal is non-empty the repeat-loop will break",
          `@range` = () => Traversal.ontology :: Nil
        )
    val untilTraversal: TypedProperty[Node] = until.property + Traversal.ontology

    object max
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Repeat/max",
          "max",
          "The maximum number of repeats",
          `@range` = () => DataType.default.`@string` :: Nil
        )
    val maxInt: TypedProperty[Int] = max.property + DataType.default.`@int`

    object collect
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Repeat/collect",
          "collect",
          "Set to true to return all intermediate results (of each repeat)",
          `@range` = () => DataType.default.`@string` :: Nil
        )
    val collectBoolean: TypedProperty[Boolean] = collect.property + DataType.default.`@boolean`
  }
  override lazy val properties
    : List[Property] = keys.traversal.property :: keys.until.property :: keys.max.property :: keys.collect.property :: Nil

  def apply[E <: ClassType[_]](traversal: Traversal[_ <: ClassType[_], E, _ <: HList],
                               until: Option[Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]],
                               max: Option[Int] = None,
                               collect: Option[Boolean] = None): Repeat[E] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.traversalTraversal, traversal.self)
    until.foreach(until => node.addOut(keys.untilTraversal, until.self))
    max.foreach(max => node.addOut(keys.maxInt, max))
    collect.foreach(collect => node.addOut(keys.collectBoolean, collect))
    Repeat(traversal, until, max, collect, node)
  }

}

case class Repeat[E <: ClassType[_]] private (
    traversal: Traversal[_ <: ClassType[_], E, _ <: HList],
    until: Option[Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]],
    max: Option[Int],
    collect: Option[Boolean],
    override val value: Node)
    extends WrappedNode(value)
    with BranchStep {
  override def prettyPrint: String =
    s"repeat(_.${traversal.toString}" + (until match {
      case Some(until) =>
        ", " + until + (max match {
          case Some(max) =>
            ", " + max + (collect match {
              case Some(collect) => ", collect = " + collect
              case None          => ""
            })
          case None =>
            collect match {
              case Some(collect) => ", collect = " + collect
              case None          => ""
            }
        })
      case None =>
        max match {
          case Some(max) =>
            ", max = " + max + (collect match {
              case Some(collect) => ", collect = " + collect
              case None          => ""
            })
          case None =>
            collect match {
              case Some(collect) => ", collect = " + collect
              case None          => ""
            }
        }
    }) + ")"
}
