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
              .wrap(_)(DetachedGraph)(ClassType.default[Any])
              .asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
          .head,
        node
          .out(keys.untilTraversal)
          .take(1)
          .map(
            Traversal
              .wrap(_)(DetachedGraph)(ClassType.default[Any])
              .asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
          .headOption,
        node.out(keys.maxInt).headOption,
        node.out(keys.collectBoolean).headOption,
        node
      )
  }

  object keys {
    private val traversalNode = MemGraphDefault.ns.upsertNode("sptth/tbd.tld/librarian/step/Repeat/traversal")
    traversalNode.addLabel(Property.ontology)
    traversalNode --- Property.default.label --> "traversal" --- Property.default.language --> "en"
    traversalNode --- Property.default.comment --> "A traversal .." --- Property.default.language --> "en"
    traversalNode --- Property.default.range --> Traversal.ontology
    lazy val traversal: Property                = Property(traversalNode)
    val traversalTraversal: TypedProperty[Node] = traversal + Traversal.ontology

    private val untilNode = MemGraphDefault.ns.upsertNode("sptth/tbd.tld/librarian/step/Repeat/until")
    untilNode.addLabel(Property.ontology)
    untilNode --- Property.default.label --> "until" --- Property.default.language --> "en"
    untilNode --- Property.default.comment --> "If the result of this traversal is non-empty the repeat-loop will break" --- Property.default.language --> "en"
    untilNode --- Property.default.range --> Traversal.ontology
    lazy val until: Property                = Property(untilNode)
    val untilTraversal: TypedProperty[Node] = until + Traversal.ontology

    private val maxNode =
      MemGraphDefault.ns.upsertNode("sptth/tbd.tld/librarian/step/Repeat/max")
    maxNode.addLabel(Property.ontology)
    maxNode --- Property.default.label --> "max" --- Property.default.language --> "en"
    maxNode --- Property.default.comment --> "The maximum number of repeats" --- Property.default.language --> "en"
    maxNode --- Property.default.range --> DataType.default.intType
    lazy val max: Property         = Property(maxNode)
    val maxInt: TypedProperty[Int] = max + DataType.default.intType

    private val collectNode = MemGraphDefault.ns.upsertNode("sptth/tbd.tld/librarian/step/Repeat/collect")
    collectNode.addLabel(Property.ontology)
    collectNode --- Property.default.label --> "collect" --- Property.default.language --> "en"
    collectNode --- Property.default.comment --> "Set to true to return all intermediate results (of each repeat)" --- Property.default.language --> "en"
    collectNode --- Property.default.range --> DataType.default.boolType
    lazy val collect: Property                 = Property(collectNode)
    val collectBoolean: TypedProperty[Boolean] = collect + DataType.default.boolType
  }

  def apply[E <: ClassType[_]](traversal: Traversal[_ <: ClassType[_], E, _ <: HList],
                               until: Option[Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]],
                               max: Option[Int] = None,
                               collect: Option[Boolean] = None): Repeat[E] = {
    val node = DetachedGraph.createNode(ontology)

    node.addOut(keys.traversalTraversal, traversal.self)
    until.foreach(until => node.addOut(keys.untilTraversal, until.self))
    max.foreach(max => node.addOut(keys.maxInt, max))
    collect.foreach(collect => node.addOut(keys.collectBoolean, collect))
    Repeat(traversal, until, max, collect, node)
  }

  ontologyNode --- Property.default.properties --> keys.traversal
  ontologyNode --- Property.default.properties --> keys.until
  ontologyNode --- Property.default.properties --> keys.max
  ontologyNode --- Property.default.properties --> keys.collect
  //  MemGraphDefault.ns.storeOntology(ontology)
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
