package lspace.lgraph.provider.cassandra

import argonaut.Parse
import com.datastax.driver.core.PagingState
import com.outworkers.phantom.builder.batch.BatchQuery
import com.outworkers.phantom.dsl._
import lspace.lgraph._
import lspace.lgraph.store.{LEdgeStore, LNodeStore, LValueStore, StoreManager}
import lspace.librarian.datatype.DataType
import lspace.librarian.structure
import lspace.librarian.structure.{Ontology, Property}
import lspace.parse.json.JsonLD
import monix.eval.Task
import monix.reactive._

import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.concurrent.duration._

object CassandraStoreManager {
  val cassandraQueryConsumer = Consumer
    .foreach[((Session, ExecutionContextExecutor), BatchQuery[_ <: com.outworkers.phantom.builder.ConsistencyBound])] {
      case ((session: Session, ec: ExecutionContextExecutor),
            query: BatchQuery[com.outworkers.phantom.builder.ConsistencyBound]) =>
        Await.result(query.future()(session, ec), 60 seconds)
    }
  val loadBalancer = {
    Consumer
      .loadBalance(parallelism = 50, cassandraQueryConsumer)
  }
  monix.eval.TaskLift
}

class CassandraStoreManager[G <: LGraph](override val graph: G, override val database: CassandraGraphTables)
    extends StoreManager(graph)
    with DatabaseProvider[CassandraGraphTables] {
  import CassandraStoreManager._

  override def nodeStore: LNodeStore[G]   = graph.nodeStore.asInstanceOf[LNodeStore[G]]
  override def edgeStore: LEdgeStore[G]   = graph.edgeStore.asInstanceOf[LEdgeStore[G]]
  override def valueStore: LValueStore[G] = graph.valueStore.asInstanceOf[LValueStore[G]]

  val jsonld = JsonLD(graph)

  Await.result(
    Future.sequence(
      Seq(
        database.nodes.create.ifNotExists().future(),
        database.nodesByIri.create.ifNotExists().future(),
        database.nodesByIris.create.ifNotExists().future(),
        database.edges.create.ifNotExists().future(),
        database.edgesByIri.create.ifNotExists().future(),
        database.edgesByFrom.create.ifNotExists().future(),
        database.edgesByFromAndKey.create.ifNotExists().future(),
        database.edgesByFromAndKeyAndTo.create.ifNotExists().future(),
        database.edgesByFromAndTo.create.ifNotExists().future(),
        database.edgesByToAndKey.create.ifNotExists().future(),
        database.edgesByTo.create.ifNotExists().future(),
        database.values.create.ifNotExists().future(),
        database.valuesByIri.create.ifNotExists().future(),
        database.valuesByValue.create.ifNotExists().future()
      )),
    60 seconds
  )

  private def pagedNodesF(f: () => ListResult[Node], pagingState: Option[PagingState] = None): Stream[graph.GNode] = {
    val result = f()
    result.records.toStream
      .filterNot(n => graph.nodeStore.isDeleted(n.id))
      .map(node =>
        nodeStore.cachedById(node.id).asInstanceOf[Option[graph.GNode]].getOrElse {
          val _node = graph.newNode(node.id)

          node.labels
            .flatMap { id =>
              Ontology.allOntologies.byId.get(id).orElse(graph.ns.ontologies.get(id))
            }
            .foreach(o => _node.addLabel(o))
          _node
      }) #::: (if (!result.result.isExhausted()) pagedNodesF(f, Some(result.pagingState)) else Stream())
  }
  private def pagedEdgesF(f: () => ListResult[Edge],
                          pagingState: Option[PagingState] = None): Stream[graph.GEdge[Any, Any]] = {
    val result = f()

    result.records.toStream
      .filterNot(
        e =>
          graph.edgeStore.isDeleted(e.id) || graph.edgeStore.isDeleted(e.fromId) || graph.edgeStore
            .isDeleted(e.toId) || graph.nodeStore.isDeleted(e.fromId) || graph.valueStore
            .isDeleted(e.fromId) || graph.nodeStore.isDeleted(e.toId) || graph.valueStore.isDeleted(e.toId))
      .map(edge =>
        edgeStore.cachedById(edge.id).asInstanceOf[Option[graph.GEdge[Any, Any]]].getOrElse {
          val from = edge.fromType match {
            case 0 => nodeStore.cachedById(edge.fromId).getOrElse(nodeStore.hasId(edge.fromId).get)
//              .getOrElse(LNode(edge.from, graph).asInstanceOf[graph._Node])
            case 1 => edgeStore.cachedById(edge.fromId).getOrElse(edgeStore.hasId(edge.fromId).get)
            case 2 => valueStore.cachedById(edge.fromId).getOrElse(valueStore.hasId(edge.fromId).get)
          }
          val to = edge.toType match {
            case 0 => nodeStore.cachedById(edge.toId).getOrElse(nodeStore.hasId(edge.toId).get)
            case 1 => edgeStore.cachedById(edge.toId).getOrElse(edgeStore.hasId(edge.toId).get)
            case 2 => valueStore.cachedById(edge.toId).getOrElse(valueStore.hasId(edge.toId).get)
          }
//        println("pagedEdgesF " + edge.key)
          graph.newEdge(
            edge.id,
            from.asInstanceOf[graph.GResource[Any]],
            Property.allProperties.byId.get(edge.key).orElse(graph.ns.properties.get(edge.key)).get,
            to.asInstanceOf[graph.GResource[Any]]
          )
      }) #::: (if (!result.result.isExhausted()) pagedEdgesF(f, Some(result.pagingState)) else Stream())
  }
  private def pagedValuesF(f: () => ListResult[Value],
                           pagingState: Option[PagingState] = None): Stream[graph.GValue[Any]] = {
    val result = f()
    result.records.toStream
      .filterNot(v => graph.valueStore.isDeleted(v.id))
      .map(value =>
        valueStore.cachedById(value.id).asInstanceOf[Option[graph.GValue[Any]]].getOrElse {
          val datatype    = DataType.allDataTypes.byId.get(value.label).orElse(graph.ns.datatypes.get(value.label)).get
          val parsedValue = jsonld.jsonToValue(datatype, Parse.parseOption(value.value).get).get._2
          graph.newValue(value.id, parsedValue, datatype)
      }) #::: (if (!result.result.isExhausted()) pagedValuesF(f, Some(result.pagingState)) else Stream())
  }

  override def nodeById(id: Long): Option[graph.GNode] =
    pagedNodesF(
      () =>
        Await
          .result(database.nodes.findById(id), 5 seconds)).headOption

  override def nodesById(ids: List[Long]): Stream[graph.GNode] =
    pagedNodesF(
      () =>
        Await
          .result(database.nodes.findByIds(ids), 5 seconds))

  override def nodeByIri(iri: String): Stream[graph.GNode] =
    pagedNodesF(
      () =>
        Await
          .result(database.nodesByIri.findByIris(graph.valueStore.byValue(iri).map(_.id).toList), 5 seconds))

  override def nodesByIri(iris: List[String]): Stream[graph.GNode] =
    pagedNodesF(
      () =>
        Await
          .result(database.nodesByIri.findByIris(iris.flatMap(iri => graph.valueStore.byValue(iri).map(_.id).toList)),
                  5 seconds))

  override def edgeById(id: Long): Option[graph.GEdge[Any, Any]] =
    pagedEdgesF(
      () =>
        Await
          .result(database.edges.findById(id), 5 seconds)).headOption

  override def edgesById(ids: List[Long]): Stream[graph.GEdge[Any, Any]] =
    pagedEdgesF(
      () =>
        Await
          .result(database.edges.findByIds(ids), 5 seconds))

  override def edgesByFromId(fromId: Long): Stream[graph.GEdge[Any, Any]] =
    pagedEdgesF(
      () =>
        Await
          .result(database.edgesByFrom.findByFrom(fromId), 5 seconds))

  override def edgesByFromIdAndKey(fromId: Long, key: Property): Stream[graph.GEdge[Any, Any]] =
    Property.allProperties.idByIri
      .get(key.iri)
      .orElse(
        graph.ns.nodes
          .hasIri(key.iri)
          .headOption
          .map(_.id))
      .map(keyId =>
        pagedEdgesF(() =>
          Await
            .result(database.edgesByFromAndKey.findByFromAndKey(fromId, keyId), 5 seconds)))
      .getOrElse(Stream())

  override def edgesByToId(toId: Long): Stream[graph.GEdge[Any, Any]] =
    pagedEdgesF(
      () =>
        Await
          .result(database.edgesByTo.findByTo(toId), 5 seconds))

  override def edgesByToIdAndKey(toId: Long, key: Property): Stream[graph.GEdge[Any, Any]] =
    Property.allProperties.idByIri
      .get(key.iri)
      .orElse(
        graph.ns.nodes
          .hasIri(key.iri)
          .headOption
          .map(_.id))
      .map(keyId =>
        pagedEdgesF(() =>
          Await
            .result(database.edgesByToAndKey.findByToAndKey(toId, keyId), 5 seconds)))
      .getOrElse(Stream())

  override def edgesByFromIdAndToId(fromId: Long, toId: Long): Stream[graph.GEdge[Any, Any]] =
    pagedEdgesF(
      () =>
        Await
          .result(database.edgesByFromAndTo.findByFromAndKeyAndTo(fromId, toId), 5 seconds))

  override def edgesByFromIdAndKeyAndToId(fromId: Long, key: Property, toId: Long): Stream[graph.GEdge[Any, Any]] =
    Property.allProperties.idByIri
      .get(key.iri)
      .orElse(
        graph.ns.nodes
          .hasIri(key.iri)
          .headOption
          .map(_.id))
      .map(keyId =>
        pagedEdgesF(() =>
          Await
            .result(database.edgesByFromAndKeyAndTo.findByFromAndKeyAndTo(fromId, keyId, toId), 5 seconds)))
      .getOrElse(Stream())

//  override def edgesByKey(key: Property): Stream[graph.GEdge[_, _]] =
//    pagedEdgesF(
//      () =>
//        Await
//          .result(database.edgesByKey.findById(ids), 5 seconds))

  override def edgeByIri(iri: String): Stream[graph.GEdge[Any, Any]] =
    pagedEdgesF(
      () =>
        Await
          .result(database.edgesByIri.findByIris(graph.valueStore.byValue(iri).map(_.id).toList), 5 seconds))

  override def edgesByIri(iris: List[String]): Stream[graph.GEdge[Any, Any]] =
    pagedEdgesF(
      () =>
        Await
          .result(database.edgesByIri.findByIris(iris.flatMap(iri => graph.valueStore.byValue(iri).map(_.id).toList)),
                  5 seconds))

  override def valueById(id: Long): Option[graph.GValue[Any]] =
    pagedValuesF(
      () =>
        Await
          .result(database.values.findById(id), 5 seconds)).headOption

  override def valuesById(ids: List[Long]): Stream[graph.GValue[Any]] =
    pagedValuesF(
      () =>
        Await
          .result(database.values.findByIds(ids), 5 seconds))

  override def valueByIri(iri: String): Stream[graph.GValue[Any]] =
    pagedValuesF(
      () =>
        Await
          .result(database.valuesByIri.findByIris(graph.valueStore.byValue(iri).map(_.id).toList), 5 seconds))

  override def valuesByIri(iris: List[String]): Stream[graph.GValue[Any]] =
    pagedValuesF(
      () =>
        Await
          .result(database.valuesByIri.findByIris(iris.flatMap(iri => graph.valueStore.byValue(iri).map(_.id).toList)),
                  5 seconds))

  override def valueByValue[T](value: T, dt: DataType[T]): Stream[graph.GValue[T]] =
    pagedValuesF(
      () =>
        Await
          .result(database.valuesByValue.findByValue(jsonld.anyToJson(value, List(dt))._1.toString()), 5 seconds))
      .asInstanceOf[Stream[graph.GValue[T]]]

  override def valuesByValue[T](values: List[(T, DataType[T])]): Stream[graph.GValue[T]] =
    pagedValuesF(
      () =>
        Await
          .result(database.valuesByValue.findByValues(values.map {
            case (value, dt) => jsonld.anyToJson(value, List(dt))._1.toString()
          }), 5 seconds))
      .asInstanceOf[Stream[graph.GValue[T]]]

  private def structureNodeToNode(node: graph._Node) = Node(
    node.id,
    node.outE(Property.default.`@id`).headOption.map(_.to.id).getOrElse(0l),
    node.outE(Property.default.`@ids`).map(_.to.id).toSet,
    node.labels.map(
      o =>
        Ontology.allOntologies.idByIri
          .getOrElse(o.iri,
                     graph.ns.nodes
                       .hasIri(o.iri)
                       .headOption
                       .getOrElse(graph.ns.ontologies.store(o))
                       .id))
  )
  override def storeNodes(nodes: List[graph.GNode]): Task[_] = loadBalancer.synchronized {
    val cnodes = nodes.map(structureNodeToNode)

    Observable
      .fromIterable(
        cnodes.map(database.nodes.store(_)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
          cnodes.map(database.nodesByIri.store(_)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
          cnodes
            .flatMap(n => n.iris.map(iri => n.copy(iri = iri)))
            .map(database.nodesByIris.store(_))
            .grouped(100)
            .toStream
            .map(Batch.unlogged.add(_))
          map (r => (session, context) -> r.asInstanceOf[BatchQuery[com.outworkers.phantom.builder.ConsistencyBound]])
      )
      .consumeWith(loadBalancer)

  }

  private def structureEdgeToEdge(edge: graph.GEdge[_, _]) = Edge(
    edge.id,
    edge.outE(Property.default.`@id`).headOption.map(_.to.id).getOrElse(0l),
    edge.outE(Property.default.`@ids`).map(_.to.id).toSet,
    edge.from.id,
    edge.from match {
      case n: structure.Node       => 0
      case e: structure.Edge[_, _] => 1
      case v: structure.Value[_]   => 2
    },
    Property.allProperties.idByIri
      .getOrElse(edge.key.iri,
                 graph.ns.nodes
                   .hasIri(edge.key.iri)
                   .headOption
                   .getOrElse(graph.ns.properties.store(edge.key))
                   .id),
    edge.to.id,
    edge.to match {
      case n: structure.Node       => 0
      case e: structure.Edge[_, _] => 1
      case v: structure.Value[_]   => 2
    }
  )
  override def storeEdges(edges: List[graph.GEdge[_, _]]): Task[_] = loadBalancer.synchronized {
    val cedges = edges.map(structureEdgeToEdge)
    Observable
      .fromIterable(
        cedges.map(database.edges.store(_)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
          cedges.map(database.edgesByIri.store(_)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
          cedges.map(database.edgesByFrom.store(_)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
          cedges.map(database.edgesByTo.store(_)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
          cedges.map(database.edgesByFromAndTo.store(_)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
          cedges.map(database.edgesByFromAndKey.store(_)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
          cedges.map(database.edgesByToAndKey.store(_)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
          cedges.map(database.edgesByFromAndKeyAndTo.store(_)).grouped(100).toStream.map(Batch.unlogged.add(_))
          map (r => (session, context) -> r.asInstanceOf[BatchQuery[com.outworkers.phantom.builder.ConsistencyBound]])
      )
      .consumeWith(loadBalancer)

//    Await.result(
//      Future.sequence(
//        Seq(
//          database.edges.storeRecords(cedges),
//          database.edgesByIri.storeRecords(cedges),
//          database.edgesByFrom.storeRecords(cedges),
//          database.edgesByTo.storeRecords(cedges),
//          database.edgesByFromAndTo.storeRecords(cedges),
//          database.edgesByFromAndKey.storeRecords(cedges),
//          database.edgesByToAndKey.storeRecords(cedges),
//          database.edgesByFromAndKeyAndTo.storeRecords(cedges)
//        )
//      ),
//      60 seconds
//    )
  }

  private def structureValueToValue(value: graph.GValue[_]) = Value(
    value.id,
    value.outE(Property.default.`@id`).headOption.map(_.to.id).getOrElse(0l),
    value.outE(Property.default.`@ids`).map(_.to.id).toSet,
    DataType.allDataTypes.idByIri
      .getOrElse(value.label.iri,
                 graph.ns.nodes
                   .hasIri(value.label.iri)
                   .headOption
                   .getOrElse(graph.ns.datatypes.store(value.label))
                   .id),
    jsonld.anyToJson(value.value, List(value.label))._1.toString()
  )

  override def storeValues(values: List[graph.GValue[_]]): Task[_] = loadBalancer.synchronized {
    val cvalues = values.map(structureValueToValue)

    Observable
      .fromIterable(
        cvalues.map(database.values.store(_)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
          cvalues.map(database.valuesByIri.store(_)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
          cvalues.map(database.valuesByValue.store(_)).grouped(100).toStream.map(Batch.unlogged.add(_))
          map (r => (session, context) -> r.asInstanceOf[BatchQuery[com.outworkers.phantom.builder.ConsistencyBound]])
      )
      .consumeWith(loadBalancer)
  }

  def deleteNodes(nodes: List[graph.GNode]): Task[_] = {
    val cnodes = nodes.map(structureNodeToNode) //ignores unretrievable nodes
    Observable
      .fromIterable(
        cnodes.map(n => database.nodes.delete(n.id)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
          cnodes.map(n => database.nodesByIri.delete(n.id, n.iri)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
          cnodes
            .flatMap(n => n.iris.map(iri => database.nodesByIris.delete(n.id, iri)))
            .grouped(100)
            .toStream
            .map(Batch.unlogged.add(_))
          map (r => (session, context) -> r.asInstanceOf[BatchQuery[com.outworkers.phantom.builder.ConsistencyBound]])
      )
      .consumeWith(loadBalancer)
  }
  def deleteEdges(edges: List[graph.GEdge[_, _]]): Task[_] = {
    val cedges = edges.map(structureEdgeToEdge) //ignores unretrievable edges
    Observable
      .fromIterable(
        cedges.map(e => database.edges.delete(e.id)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
          cedges
            .filter(_.iri != 0l)
            .map(e => database.edgesByIri.delete(e.id, e.iri))
            .grouped(100)
            .toStream
            .map(Batch.unlogged.add(_)) ++
          cedges
            .filter(_.iris.nonEmpty)
            .map(e => database.edgesByFrom.delete(e.id, e.fromId))
            .grouped(100)
            .toStream
            .map(Batch.unlogged.add(_)) ++
          cedges.map(e => database.edgesByTo.delete(e.id, e.toId)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
          cedges
            .map(e => database.edgesByFromAndTo.delete(e.id, e.fromId, e.toId))
            .grouped(100)
            .toStream
            .map(Batch.unlogged.add(_)) ++
          cedges
            .map(e => database.edgesByFromAndKey.delete(e.id, e.fromId, e.key))
            .grouped(100)
            .toStream
            .map(Batch.unlogged.add(_)) ++
          cedges
            .map(e => database.edgesByToAndKey.delete(e.id, e.toId, e.key))
            .grouped(100)
            .toStream
            .map(Batch.unlogged.add(_)) ++
          cedges
            .map(e => database.edgesByFromAndKeyAndTo.delete(e.id, e.fromId, e.key, e.toId))
            .grouped(100)
            .toStream
            .map(Batch.unlogged.add(_))
          map (r => (session, context) -> r.asInstanceOf[BatchQuery[com.outworkers.phantom.builder.ConsistencyBound]])
      )
      .consumeWith(loadBalancer)
  }
  def deleteValues(values: List[graph.GValue[_]]): Task[_] = {
    val cvalues = values.map(structureValueToValue) //ignores unretrievable edges
    Observable
      .fromIterable(
        cvalues.map(v => database.values.delete(v.id)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
          cvalues
            .filter(_.iri != 0l)
            .map(v => database.valuesByIri.delete(v.id, v.iri))
            .grouped(100)
            .toStream
            .map(Batch.unlogged.add(_)) ++
          cvalues
            .map(v => database.valuesByValue.delete(v.id, v.value))
            .grouped(100)
            .toStream
            .map(Batch.unlogged.add(_))
          map (r => (session, context) -> r.asInstanceOf[BatchQuery[com.outworkers.phantom.builder.ConsistencyBound]])
      )
      .consumeWith(loadBalancer)
  }

  override def nodes: Stream[graph.GNode] =
    pagedNodesF(
      () =>
        Await
          .result(database.nodes.select.fetchRecord(), 5 seconds))

  def nodeCount(): Long = {
    Await
      .result(database.nodes.select.count().one(), 300 seconds)
      .get
  }

  override def edges: Stream[graph.GEdge[Any, Any]] =
    pagedEdgesF(
      () =>
        Await
          .result(database.edges.select.fetchRecord(), 5 seconds))

  def edgeCount(): Long = {
    Await
      .result(database.edges.select.count().one(), 300 seconds)
      .get
  }

  override def values: Stream[graph.GValue[Any]] =
    pagedValuesF(
      () =>
        Await
          .result(database.values.select.fetchRecord(), 5 seconds))

  def valueCount(): Long = {
    Await
      .result(database.values.select.count().one(), 300 seconds)
      .get
  }

  def close(): Unit = {
    database.shutdown()
  }

}
