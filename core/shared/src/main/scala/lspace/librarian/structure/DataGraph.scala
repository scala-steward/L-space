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

  abstract override protected def _createNode(_id: Long)(ontology: Ontology*): _Node = {
//    println(s"datagraph._createnode")
    val node = super._createNode(_id)(ontology: _*)
//    println(s"done datagraph.super._createNode")
    _storeNode(node)
    _indexNode(node)

    ontology.foreach { ontology =>
      if (!Ontology.allOntologies.byIri.contains(ontology.iri) && ns.getOntology(ontology.iri).isEmpty)
        ns.storeOntology(ontology)
    }
//    println("done datagraph._createnode")
    node
  }

  override protected def _deleteNode(node: _Node): Unit = {
    //    `@typeIndex`.delete()
    super._deleteNode(node)
  }

  protected def _indexNode(node: _Node): Unit = {
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
  abstract override protected def _createEdge[S, E](
      id: Long)(_from: _Resource[S], key: Property, _to: _Resource[E]): _Edge[S, E] = {
    val edge = super._createEdge(id)(_from, key, _to)

    if (ns.getProperty(key.iri).isEmpty) ns.storeProperty(key)

    _storeEdge(edge)
    _indexEdge(edge)

//    if (!Property.allProperties.contains(key.iri) && ns.getProperty(key.iri).isEmpty) ns.storeProperty(key)
//    println(s"${key.iri} present ${ns.getProperty(key.iri).isDefined}")

    edge
  }

  override protected def _deleteEdge(edge: _Edge[_, _]): Unit = {

    super._deleteEdge(edge)
  }

  protected def _indexEdge[S, E](edge: _Edge[S, E]): Unit = {

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

  abstract override protected def _createValue[T](_id: Long)(_value: T)(dt: DataType[T]): _Value[T] = {
//    println(s"datagraph._create value ${_id} ${_value}")
    val value = super._createValue(_id)(_value)(dt)
    _storeValue(value)
    if (dt != DataType.default.`@boolean`) _indexValue(value)

    if (!DataType.allDataTypes.byIri.contains(dt.iri) && ns.getDataType(dt.iri).isEmpty) ns.storeDataType(dt)

    value
  }

  override protected def _deleteValue(value: _Value[_]): Unit = {
    super._deleteValue(value)
  }

  protected def _indexValue(value: _Value[_]): Unit = {
    index
      .getOrCreateIndex(Set(value.label))
      .store(Vector((Map(value.label -> List(value)), value)))
  }
}
