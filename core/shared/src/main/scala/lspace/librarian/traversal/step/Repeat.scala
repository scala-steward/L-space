package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.datatype.DataType
import lspace.structure._
import monix.eval.Task
import monix.reactive.Observable
import shapeless.HList

object Repeat extends StepDef("Repeat") with StepWrapper[Repeat[ClassType[Any]]] {

  def toStep(node: Node): Task[Repeat[ClassType[Any]]] =
    for {
      by <- node
        .out(keys.traversalTraversal)
        .take(1)
        .map(
          Traversal
            .toTraversal(_)
            .map(_.asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]]))
        .head
      until <- Task.gather(
        node
          .out(keys.untilTraversal)
          .take(1)
          .map(Traversal
            .toTraversal(_)
            .map(_.asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])))
    } yield
      Repeat(
        by,
        until.headOption,
        node.out(keys.maxInt).headOption,
        node.out(keys.collectBoolean).headOption.getOrElse(false),
        node.out(keys.noloopBoolean).headOption.getOrElse(false)
      )

  object keys {
    object traversal
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Repeat/traversal",
          "by",
          "A traversal ..",
          `@range` = () => Traversal.ontology :: Nil
        )
    val traversalTraversal: TypedProperty[Node] = traversal.property as Traversal.ontology

    object until
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Repeat/until",
          "until",
          "If the result of this traversal is non-empty the repeat-loop will break",
          `@range` = () => Traversal.ontology :: Nil
        )
    val untilTraversal: TypedProperty[Node] = until.property as Traversal.ontology

    object max
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Repeat/max",
          "max",
          "The maximum number of repeats",
          `@range` = () => DataType.default.`@string` :: Nil
        )
    val maxInt: TypedProperty[Int] = max.property as DataType.default.`@int`

    object collect
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Repeat/collect",
          "collect",
          "Set to true to return all intermediate results (of each repeat)",
          `@range` = () => DataType.default.`@string` :: Nil
        )
    val collectBoolean: TypedProperty[Boolean] = collect.property as DataType.default.`@boolean`

    object noloop
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Repeat/noloop",
          "noloop",
          "Set to true to prevent running into loops/cycles",
          `@range` = () => DataType.default.`@string` :: Nil
        )
    val noloopBoolean: TypedProperty[Boolean] = collect.property as DataType.default.`@boolean`
  }
  override lazy val properties
    : List[Property] = keys.traversal.property :: keys.until.property :: keys.max.property :: keys.collect.property :: Nil

  implicit def toNode[CT0 <: ClassType[_]](step: Repeat[CT0]): Task[Node] = {
    for {
      node      <- DetachedGraph.nodes.create(ontology)
      traversal <- step.traversal.toNode
      _         <- node.addOut(keys.traversalTraversal, traversal)
      until     <- Task.gather(step.until.toList.map(_.toNode))
      _         <- Task.gather(until.map(node.addOut(keys.untilTraversal, _)))
      _         <- if (step.collect) node.addOut(keys.collectBoolean, step.collect) else Task.unit
      _         <- if (step.noloop) node.addOut(keys.collectBoolean, step.noloop) else Task.unit
    } yield node
  }.memoizeOnSuccess
}

case class Repeat[E0 <: ClassType[_]](traversal: Traversal[_ <: ClassType[_], E0, _ <: HList],
                                      until: Option[Traversal[E0, _ <: ClassType[_], _ <: HList]],
                                      max: Option[Int] = None,
                                      collect: Boolean = false,
                                      noloop: Boolean = false)
    extends BranchStep {

  lazy val toNode: Task[Node] = this
  override def prettyPrint: String =
    s"repeat(_.${traversal.toString}" + (until match {
      case Some(until) =>
        ", " + until + (max match {
          case Some(max) =>
            ", " + max + (collect match {
              case true  => ", collect = " + collect
              case false => ""
            })
          case None =>
            collect match {
              case true  => ", collect = " + collect
              case false => ""
            }
        })
      case None =>
        max match {
          case Some(max) =>
            ", max = " + max + (collect match {
              case true  => ", collect = " + collect
              case false => ""
            })
          case None =>
            collect match {
              case true  => ", collect = " + collect
              case false => ""
            }
        }
    }) + ")"
}
