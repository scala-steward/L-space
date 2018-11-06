package lspace.librarian.process.traversal

import java.time.Instant

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.types._

object Collection {
  private val ontologyNode =
    MemGraphDefault.ns.upsertNode("sptth/tbd.tld/librarian/Collection")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- Property.default.label --> "Result" --- Property.default.language --> "en"
  ontologyNode --- Property.default.comment --> "Result" --- Property.default.language --> "en"

  def wrap(node: Node): Collection[Any] = node match {
    case node: Collection[Any] => node
    case _ =>
      Collection(node.out(Collection.keys.startDateTime),
                 node.out(Collection.keys.endDateTime),
                 node.out(Collection.keys.item))(node)
  }

  val keys = new {
    private val startNode = MemGraphDefault.ns.upsertNode("sptth/tbd.tld/librarian/Collection/start")
    startNode.addLabel(Property.ontology)
    startNode --- Property.default.label --> "start" --- Property.default.language --> "en"
    startNode --- Property.default.comment --> "Any value" --- Property.default.language --> "en"
    startNode --- Property.default.range --> DataType.default.dateTimeType
    lazy val start: Property = Property(startNode)

    private val endNode =
      MemGraphDefault.ns.upsertNode("sptth/tbd.tld/librarian/Collection/end")
    endNode.addLabel(Property.ontology)
    endNode --- Property.default.label --> "end" --- Property.default.language --> "en"
    endNode --- Property.default.comment --> "Any value" --- Property.default.language --> "en"
    endNode --- Property.default.range --> DataType.default.dateTimeType
    lazy val end: Property = Property(endNode)

    private val itemNode =
      MemGraphDefault.ns.upsertNode("sptth/tbd.tld/librarian/Collection/item")
    itemNode.addLabel(Property.ontology)
    itemNode --- Property.default.label --> "item" --- Property.default.language --> "en"
    itemNode --- Property.default.comment --> "Any value" --- Property.default.language --> "en"
    itemNode --- Property.default.container --> NS.types.list
    lazy val item: Property = Property(itemNode)

    val startDateTime = start + DataType.default.dateTimeType

    val endDateTime = end + DataType.default.dateTimeType

    //    val itemString = item.addRange(item.graph.textType)
    //    val itemInt = item.addRange(item.graph.intType)
    //    val itemDouble = item.addRange(item.graph.doubleType)
    //    val itemLong = item.addRange(item.graph.longType)
    //    val itemDateTime = item.addRange(item.graph.dateTimeType)
    //    val itemDate = item.addRange(item.graph.dateType)
    //    //    val valueTime = value.addRange(DataTypes.TimeType)
    //    val itemGeo = item.addRange(item.graph.geojsonType)
    //    val itemBoolean = item.addRange(item.graph.boolType)
    //    val itemUrl = item.addRange(item.graph.uRLType)
    //    val itemEpoch = item.addRange(item.graph.epochType)
    //    val itemAny = item.addRange()
  }

  ontologyNode --- Property.default.properties --> keys.start
  ontologyNode --- Property.default.properties --> keys.end
  ontologyNode --- Property.default.properties --> keys.item

  lazy val ontology: Ontology = Ontology(ontologyNode)

  def apply[T](node: Node, ct: ClassType[T]): Collection[T] = wrap(node).asInstanceOf[Collection[T]]
  def apply[T, CT <: ClassType[T]](start: Instant, end: Instant, items: List[T])(ct: CT): Collection[T] = {
    val node = DetachedGraph.createNode(ontology)
    node.addOut(keys.start, start)
    node.addOut(keys.end, end)
    //    items.foreach(item => node.addOut(keys.item, item))
    //    node.addOuts(keys.item, items.map(item => ClassType.valueToOntologyResource(item) -> item))
    //    items.map(item => node.---(keys.item).-->(item)(ClassType.valueToOntologyResource(item)))
    items.map(item => node.addOut(keys.item, ct, item))
    Collection(List(start), List(end), items)(node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Collection[T] private (startDateTime: List[Instant], endDateTime: List[Instant], item: List[T])(
    override val value: Node)
    extends WrappedNode(value) {}
