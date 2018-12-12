package lspace.librarian.structure

import lspace.librarian.provider.mem.index
import lspace.librarian.structure.index.Index

trait DataGraph extends Graph {

  def init(): Unit = {
    ns.init()
    index.init()
  }

  def index: IndexGraph
  protected def `@idIndex`: Index
  protected def `@typeIndex`: Index

//  trait Nodes extends super.Nodes {
//    def +(label: String): Node = create(ns.getOntology(label).getOrElse(Ontology(label)))
//  }

  override protected def getOrCreateNode(id: Long): GNode = {
//    println(s"datagraph._createnode")
    val node = super.getOrCreateNode(id)
//    println(s"done datagraph.super._createNode")
//    storeNode(node)
    _indexNode(node)

//    ontology.foreach { ontology =>
//      if (!Ontology.allOntologies.byIri.contains(ontology.iri) && ns.getOntology(ontology.iri).isEmpty)
//        ns.storeOntology(ontology)
//    }
//    println("done datagraph._createnode")
    node
  }

  override protected def deleteNode(node: GNode): Unit = {
    //    `@typeIndex`.delete()
    super.deleteNode(node)
  }

  protected def _indexNode(node: GNode): Unit = {
//    `@typeIndex`.store(Vector(Map(node.labels.map(o => Property.default.`@type` -> o))))
  }

  /**
    * creates, stores and indexes an edge
    * @param from
    * @param key
    * @param to
    * @tparam S
    * @tparam E
    * @return
    */
  abstract override protected def createEdge[S, E](id: Long,
                                                   _from: GResource[S],
                                                   key: Property,
                                                   _to: GResource[E]): GEdge[S, E] = {
    val edge = super.createEdge(id, _from, key, _to)

    if (ns.getProperty(key.iri).isEmpty) ns.storeProperty(key)

//    storeEdge(edge)
    _indexEdge(edge)

//    if (!Property.allProperties.contains(key.iri) && ns.getProperty(key.iri).isEmpty) ns.storeProperty(key)
//    println(s"${key.iri} present ${ns.getProperty(key.iri).isDefined}")

    edge
  }

  override protected def deleteEdge(edge: GEdge[_, _]): Unit = {

    super.deleteEdge(edge)
  }

  protected def _indexEdge[S, E](edge: GEdge[S, E]): Unit = {

    val from = edge.from
    val key  = edge.key
    val to   = edge.to

    if (key == Property.default.`@id`) {
      `@idIndex`.store(Vector((Map(DataType.default.`@string` -> List(to)), from)))
    } else if (key == Property.default.`@ids`) {
      `@idIndex`.store(Vector((Map(DataType.default.`@string` -> List(to)), from)))
    } else {
      val kvIndex = index.getOrCreateIndex(Set(key))
      kvIndex.store(Vector((Map(key -> List(to)), from)))
      val lkvIndex = index.getOrCreateIndex(Set(Property.default.`@label`, key))
      lkvIndex.store(Vector(
        (Map(Property.default.`@label` -> from.labels.map(_.iri).flatMap(ns.nodes.hasIri(_)), key -> List(to)), from)))
    }
  }

  abstract override protected def createValue[T](_id: Long, _value: T, dt: DataType[T]): GValue[T] = {
//    println(s"datagraph._create value ${_id} ${_value}")
    val value = super.createValue(_id, _value, dt)
//    if (ns.getDataType(dt.iri).isEmpty) ns.storeDataType(dt)
//
//    storeValue(value)
    if (dt != DataType.default.`@boolean`) _indexValue(value.asInstanceOf[GValue[_]])

    value
  }

  override protected def deleteValue(value: GValue[_]): Unit = {
    super.deleteValue(value)
  }

  protected def _indexValue(value: GValue[_]): Unit = {
    index
      .getOrCreateIndex(Set(value.label))
      .store(Vector((Map(value.label -> List(value)), value)))
  }
}
