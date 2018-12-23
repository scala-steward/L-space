package lspace.librarian.process.traversal

import java.time.Instant

import lspace.NS
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure.Ontology.OntologyDef
import lspace.librarian.structure._

object Collection
    extends OntologyDef(lspace.NS.vocab.Lspace + "librarian/Collection",
                        label = "Collection",
                        comment = "Collection ..") {

  def wrap(node: Node): Collection[Any] = node match {
    case node: Collection[Any] => node
    case _ =>
      Collection(node.out(Collection.keys.startDateTime),
                 node.out(Collection.keys.endDateTime),
                 node.out(Collection.keys.item))(node)
  }

  object keys extends Step.Properties {
    object start
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/Collection/start",
          "start",
          "Start time of collecting",
          `@range` = () => DataType.default.`@datetime` :: Nil
        ) {}
    lazy val startDateTime: TypedProperty[Instant] = start.property + DataType.default.`@datetime`

    object end
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/Collection/end",
          "end",
          "End time of collecting",
          `@range` = () => DataType.default.`@datetime` :: Nil
        ) {}
    lazy val endDateTime: TypedProperty[Instant] = start.property + DataType.default.`@datetime`

    object item
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/Collection/item",
          "item",
          "Collected item",
          container = List(NS.types.`@list`),
          `@range` = () => DataType.default.`@datetime` :: Nil
        ) {}

  }

  override lazy val properties: List[Property] = keys.start.property :: keys.end.property :: keys.item.property :: Nil

  trait Properties extends Step.Properties {
    lazy val `ns.l-space.eu/librarian/Collection/start`: Property                  = keys.start
    lazy val `ns.l-space.eu/librarian/Collection/start@Instant`: TypedKey[Instant] = keys.startDateTime
    lazy val `ns.l-space.eu/librarian/Collection/end`: Property                    = keys.end
    lazy val `ns.l-space.eu/librarian/Collection/end@Instant`: TypedKey[Instant]   = keys.endDateTime
    lazy val `ns.l-space.eu/librarian/Collection/item`: Property                   = keys.item
  }

  def apply[T](node: Node, ct: ClassType[T]): Collection[T] = wrap(node).asInstanceOf[Collection[T]]
  def apply[T, CT <: ClassType[T]](start: Instant, end: Instant, items: List[T])(ct: CT): Collection[T] = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(keys.start, start)
    node.addOut(keys.end, end)
    //    items.foreach(item => node.addOut(keys.item, item))
    //    node.addOuts(keys.item, items.map(item => ClassType.valueToOntologyResource(item) -> item))
    //    items.map(item => node.---(keys.item).-->(item)(ClassType.valueToOntologyResource(item)))
    items.map(item => node.addOut(keys.item, ct, item))
    Collection(List(start), List(end), items)(node)
  }
}

case class Collection[T] private (startDateTime: List[Instant], endDateTime: List[Instant], item: List[T])(
    override val value: Node)
    extends WrappedNode(value) {}
