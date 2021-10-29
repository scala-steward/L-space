package lspace.librarian.traversal

import java.time.Instant

import lspace.datatype.{DataType, ListType}
import lspace.librarian.traversal.step.Step
import lspace.provider.detached.DetachedGraph
import lspace.structure.{OntologyDef, _}
import monix.eval.Task

object Collection
    extends OntologyDef(
      lspace.NS.vocab.Lspace + "librarian/Collection",
      label = "Collection",
      comment = "Collection .."
    ) {

  def wrap(node: Node): Collection[Any, ClassType[Any]] = node match {
    case node: Collection[Any, ClassType[Any]] @unchecked => node
    case _ =>
      Collection[Any, ClassType[Any]](
        node.out(Collection.keys.startDateTime).head,
        node.out(Collection.keys.endDateTime).head,
        node.out(Collection.keys.itemList).take(1).flatten.toList,
        Some(ClassType.stubAny)
      ) // (node)
  }

  object keys extends Step.Properties {
    object start
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/Collection/start",
          "start",
          "Start time of collecting",
          `@range` = DataType.default.`@datetime` :: Nil
        ) {}
    lazy val startDateTime: TypedProperty[Instant] = start.property.as(DataType.default.`@datetime`)

    object end
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/Collection/end",
          "end",
          "End time of collecting",
          `@range` = DataType.default.`@datetime` :: Nil
        ) {}
    lazy val endDateTime: TypedProperty[Instant] = start.property.as(DataType.default.`@datetime`)

    object item
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/Collection/item",
          "item",
          "Collected item",
          `@range` = ListType() :: Nil
        ) {}
    lazy val itemList: TypedProperty[List[Any]] = item.as(ListType())

  }

  override lazy val properties: List[Property] = keys.start.property :: keys.end.property :: keys.item.property :: Nil

  trait Properties extends Step.Properties {
    lazy val `ns.l-space.eu/librarian/Collection/start`: Property                  = keys.start
    lazy val `ns.l-space.eu/librarian/Collection/start@Instant`: TypedKey[Instant] = keys.startDateTime
    lazy val `ns.l-space.eu/librarian/Collection/end`: Property                    = keys.end
    lazy val `ns.l-space.eu/librarian/Collection/end@Instant`: TypedKey[Instant]   = keys.endDateTime
    lazy val `ns.l-space.eu/librarian/Collection/item`: Property                   = keys.item
    lazy val `ns.l-space.eu/librarian/Collection/itemList`: TypedKey[List[Any]]    = keys.itemList
  }

//  def apply[T](node: Node, ct: Option[ClassType[T]]): Collection[T, ClassType[T]] =
//    wrap(node).asInstanceOf[Collection[T, ClassType[T]]]

  implicit def toNode[T, CT <: ClassType[_]](collection: Collection[T, CT]): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.startDateTime, collection.startDateTime)
      _    <- node.addOut(keys.endDateTime, collection.endDateTime)
      _ <- collection.ct
        .asInstanceOf[Option[ClassType[T]]]
        .map(ct => node.addOut(keys.item.property, ListType(ct).asInstanceOf[ClassType[List[T]]], collection.item))
        .getOrElse(
          node.addOut(keys.itemList, collection.item.asInstanceOf[List[Any]]).asInstanceOf[Task[Edge[Node, Any]]]
        )
    } yield node
  }.memoizeOnSuccess
}

case class Collection[+T, CT <: ClassType[_]](
  startDateTime: Instant,
  endDateTime: Instant,
  item: List[T],
  ct: Option[CT] = None
) {
  lazy val toNode: Task[Node] = this
}
