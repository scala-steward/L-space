package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.datatype.DataType
import lspace.structure._
import shapeless.HList

object Repeat extends StepDef("Repeat") with StepWrapper[Repeat[ClassType[Any]]] {

  def toStep(node: Node): Repeat[ClassType[Any]] = Repeat(
    node
      .out(keys.traversalTraversal)
      .take(1)
      .map(
        Traversal
          .toTraversal(_)
          .asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
      .head,
    node
      .out(keys.untilTraversal)
      .take(1)
      .map(
        Traversal
          .toTraversal(_)
          .asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
      .headOption,
    node.out(keys.maxInt).headOption,
    node.out(keys.collectBoolean).headOption
  )

  object keys {
    object traversal
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Repeat/traversal",
          "by",
          "A traversal ..",
          `@range` = () => Traversal.ontology :: Nil
        )
    val traversalTraversal: TypedProperty[Node] = traversal.property + Traversal.ontology

    object until
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Repeat/until",
          "until",
          "If the result of this traversal is non-empty the repeat-loop will break",
          `@range` = () => Traversal.ontology :: Nil
        )
    val untilTraversal: TypedProperty[Node] = until.property + Traversal.ontology

    object max
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Repeat/max",
          "max",
          "The maximum number of repeats",
          `@range` = () => DataType.default.`@string` :: Nil
        )
    val maxInt: TypedProperty[Int] = max.property + DataType.default.`@int`

    object collect
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Repeat/collect",
          "collect",
          "Set to true to return all intermediate results (of each repeat)",
          `@range` = () => DataType.default.`@string` :: Nil
        )
    val collectBoolean: TypedProperty[Boolean] = collect.property + DataType.default.`@boolean`
  }
  override lazy val properties
    : List[Property] = keys.traversal.property :: keys.until.property :: keys.max.property :: keys.collect.property :: Nil

  implicit def toNode[CT0 <: ClassType[_]](repeat: Repeat[CT0]): Node = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.traversalTraversal, repeat.traversal.toNode)
    repeat.until.foreach(until => node.addOut(keys.untilTraversal, until.toNode))
    repeat.max.foreach(max => node.addOut(keys.maxInt, max))
    repeat.collect.foreach(collect => node.addOut(keys.collectBoolean, collect))
    node
  }
}

case class Repeat[E0 <: ClassType[_]](traversal: Traversal[_ <: ClassType[_], E0, _ <: HList],
                                      until: Option[Traversal[E0, _ <: ClassType[_], _ <: HList]],
                                      max: Option[Int],
                                      collect: Option[Boolean])
    extends BranchStep {

  lazy val toNode: Node = this
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
