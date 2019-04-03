package lspace.lgraph.provider.cassandra

import com.datastax.driver.core.PagingState
import com.outworkers.phantom.builder.batch.BatchQuery
import com.outworkers.phantom.dsl._
import lspace.codec.{ActiveContext, NativeTypeDecoder, NativeTypeEncoder}
import lspace.lgraph._
import lspace.lgraph.store.{LEdgeStore, LNodeStore, LValueStore, StoreManager}
import lspace.datatype.DataType
import lspace.lgraph.provider.file.{DecodeLDFS, EncodeLDFS}
import lspace.structure
import lspace.structure.{Ontology, Property}
import monix.eval.Task
import monix.reactive._

import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.concurrent.duration._

object CassandraStoreManager {
  val cassandraQueryConsumer =
    Consumer
      .foreachTask[((Session, ExecutionContextExecutor),
                    BatchQuery[_ <: com.outworkers.phantom.builder.ConsistencyBound])] {
        case ((session: Session, ec: ExecutionContextExecutor),
              query: BatchQuery[com.outworkers.phantom.builder.ConsistencyBound]) =>
          for { _ <- Task.deferFuture(query.future()(session, ec)) } yield ()
      }
  val loadBalancer = {
    Consumer
      .loadBalance(parallelism = 20, cassandraQueryConsumer)
  }
}

class CassandraStoreManager[G <: LGraph, Json](override val graph: G, override val database: CassandraGraphTables)(
    implicit baseEncoder: NativeTypeEncoder.Aux[Json],
    baseDecoder: NativeTypeDecoder.Aux[Json])
    extends StoreManager(graph)
    with DatabaseProvider[CassandraGraphTables] {
  import CassandraStoreManager._

  val encoder: EncodeLDFS[Json] = EncodeLDFS()
  val decoder: DecodeLDFS[Json] = DecodeLDFS(graph)
  import decoder.{baseDecoder => _, Json => _, _}
  import encoder.{baseEncoder => _, Json => _, _}

  override def nodeStore: LNodeStore[G]   = graph.nodeStore.asInstanceOf[LNodeStore[G]]
  override def edgeStore: LEdgeStore[G]   = graph.edgeStore.asInstanceOf[LEdgeStore[G]]
  override def valueStore: LValueStore[G] = graph.valueStore.asInstanceOf[LValueStore[G]]

  private def pagedNodesF(f: Option[PagingState] => Task[ListResult[Node]],
                          pagingState: Option[PagingState] = None): Observable[graph.GNode] = {

    val resultF = f(pagingState).memoizeOnSuccess
    Observable
      .fromTask(
        for {
          result <- resultF
        } yield
          result.records.toStream
            .filterNot(n => graph.nodeStore.isDeleted(n.id)))
      .flatMap(Observable.fromIterable(_))
      .mapEval { node =>
        nodeStore.cachedById(node.id).asInstanceOf[Option[graph.GNode]].map(Task.now).getOrElse {
          val _node = graph.newNode(node.id)

          for {
            _ <- Task.now(node.labels.flatMap(graph.ns.ontologies.cached(_)).map(_node._cacheLabel))
            _ = for {
              iri            <- node.iri
              (edgeId, toId) <- node.iriEdge
            } yield
              graph.newEdge[Any, String](edgeId,
                                         _node,
                                         lspace.Label.P.`@id`,
                                         graph.newValue(toId, iri, lspace.Label.D.`@string`))
            _ = (node.iris.toList.sorted zip node.irisEdges).map {
              case (iri, (edgeId, toId)) =>
                graph.newEdge[Any, String](edgeId,
                                           _node,
                                           lspace.Label.P.`@ids`,
                                           graph.newValue(toId, iri, lspace.Label.D.`@string`))
            }
          } yield _node
        }
      } ++ Observable.fromTask(resultF).flatMap { result =>
      (if (!result.result.isExhausted()) pagedNodesF(f, Some(result.pagingState)) else Observable())
    }
  }

  private def pagedEdgesF(f: Option[PagingState] => Task[ListResult[Edge]],
                          pagingState: Option[PagingState] = None): Observable[graph.GEdge[Any, Any]] = {
    val resultF = f(pagingState)

    Observable
      .fromTask(
        for {
          result <- resultF
        } yield
          result.records.toStream
            .filterNot(e =>
              graph.edgeStore.isDeleted(e.id) || graph.edgeStore.isDeleted(e.fromId) || graph.edgeStore
                .isDeleted(e.toId) || graph.nodeStore.isDeleted(e.fromId) || graph.valueStore
                .isDeleted(e.fromId) || graph.nodeStore.isDeleted(e.toId) || graph.valueStore.isDeleted(e.toId)))
      .flatMap(Observable.fromIterable(_))
      .mapEval { edge => //map with Either? batch retrieval of uncached resources? async connect?
        edgeStore.cachedById(edge.id).asInstanceOf[Option[graph.GEdge[Any, Any]]].map(Task.now).getOrElse {
          for {
            from <- edge.fromType match {
              case 0 =>
                nodeStore.cachedById(edge.fromId).map(Task.now).getOrElse(nodeStore.hasId(edge.fromId).map(_.get))
              //              .getOrElse(LNode(edge.from, graph).asInstanceOf[graph._Node])
              case 1 =>
                edgeStore.cachedById(edge.fromId).map(Task.now).getOrElse(edgeStore.hasId(edge.fromId).map(_.get))
              case 2 =>
                valueStore.cachedById(edge.fromId).map(Task.now).getOrElse(valueStore.hasId(edge.fromId).map(_.get))
            }
            to <- edge.toType match {
              case 0 => nodeStore.cachedById(edge.toId).map(Task.now).getOrElse(nodeStore.hasId(edge.toId).map(_.get))
              case 1 => edgeStore.cachedById(edge.toId).map(Task.now).getOrElse(edgeStore.hasId(edge.toId).map(_.get))
              case 2 => valueStore.cachedById(edge.toId).map(Task.now).getOrElse(valueStore.hasId(edge.toId).map(_.get))
            }
          } yield {
            val _edge = graph.newEdge(
              edge.id,
              from.asInstanceOf[graph._Resource[Any]],
              graph.ns.properties.cached(edge.key).get,
              to.asInstanceOf[graph._Resource[Any]]
            )
            for {
              iri            <- edge.iri
              (edgeId, toId) <- edge.iriEdge
            } yield
              graph.newEdge[Any, String](edgeId,
                                         _edge,
                                         lspace.Label.P.`@id`,
                                         graph.newValue(toId, iri, lspace.Label.D.`@string`))

            (edge.iris.toList.sorted zip edge.irisEdges).map {
              case (iri, (edgeId, toId)) =>
                graph.newEdge[Any, String](edgeId,
                                           _edge,
                                           lspace.Label.P.`@ids`,
                                           graph.newValue(toId, iri, lspace.Label.D.`@string`))
            }
            _edge
          }
        }
      } ++ Observable.fromTask(resultF).flatMap { result =>
      (if (!result.result.isExhausted()) pagedEdgesF(f, Some(result.pagingState)) else Observable())
    }
  }

  private def pagedValuesF(f: Option[PagingState] => Task[ListResult[Value]],
                           pagingState: Option[PagingState] = None): Observable[graph.GValue[Any]] = {
    val resultF = f(pagingState)
    Observable
      .fromTask(
        for {
          result <- resultF
        } yield
          result.records.toStream
            .filterNot(n => graph.valueStore.isDeleted(n.id)))
      .flatMap(Observable.fromIterable(_))
      .mapEval { value =>
        valueStore.cachedById(value.id).asInstanceOf[Option[graph.GValue[Any]]].map(Task.now).getOrElse {
          val datatype = graph.ns.datatypes.cached(value.label).get
          for {
            context <- decoder.parse(value.context0).map(_.obj).flatMap {
              case Some(obj) => new decoder.WithObj(obj).extractContext(ActiveContext())
              case None      => Task.now(ActiveContext())
            }
            parsedValue <- decoder
              .parse(value.value)
              .flatMap(decoder
                .toData(_, datatype)(context))
          } yield {
            val _value = graph.newValue(value.id, parsedValue, datatype)
            for {
              iri            <- value.iri
              (edgeId, toId) <- value.iriEdge
            } yield
              graph.newEdge[Any, String](edgeId,
                                         _value,
                                         lspace.Label.P.`@id`,
                                         graph.newValue(toId, iri, lspace.Label.D.`@string`))
            (value.iris.toList.sorted zip value.irisEdges).map {
              case (iri, (edgeId, toId)) =>
                graph.newEdge[Any, String](edgeId,
                                           _value,
                                           lspace.Label.P.`@ids`,
                                           graph.newValue(toId, iri, lspace.Label.D.`@string`))
            }
            _value
          }
        }
      } ++ Observable.fromTask(resultF).flatMap { result =>
      (if (!result.result.isExhausted()) pagedValuesF(f, Some(result.pagingState)) else Observable())
    }
  }

  override def nodeById(id: Long): Task[Option[graph.GNode]] =
    pagedNodesF(ps => Task.deferFuture(database.nodes.findById(id))).headOptionL

  override def nodesById(ids: List[Long]): Observable[graph.GNode] =
    pagedNodesF(ps => Task.deferFuture(database.nodes.findByIds(ids, ps)))

//  override def nodeByIri(iri: String): Observable[graph.GNode] =
//    for {
////      iriIds <- graph.valueStore.byValue(iri).map(_.id)
//      nodes <- pagedNodesF(ps => Task.deferFuture(database.nodesByIri.findByIris(iri :: Nil, ps)))
//    } yield nodes
//
//  override def nodesByIri(iris: List[String]): Observable[graph.GNode] =
//    for {
////      iriIds <- Observable
////        .fromIterable(iris)
////        .flatMap(iri => graph.valueStore.byValue(iri).map(_.id)) //graph.valueStore.byValue(iri).map(_.id)
//      nodes <- pagedNodesF(ps => Task.deferFuture(database.nodesByIri.findByIris(iris, ps)))
//    } yield nodes

  override def edgeById(id: Long): Task[Option[graph.GEdge[Any, Any]]] =
    pagedEdgesF(ps => Task.deferFuture(database.edges.findById(id))).headOptionL

  override def edgesById(ids: List[Long]): Observable[graph.GEdge[Any, Any]] =
    pagedEdgesF(ps => Task.deferFuture(database.edges.findByIds(ids, ps)))

  override def edgesByFromId(fromId: Long): Observable[graph.GEdge[Any, Any]] =
    pagedEdgesF(ps => Task.deferFuture(database.edgesByFrom.findByFrom(fromId, ps)))

  override def edgesByFromIdAndKey(fromId: Long, key: Property): Observable[graph.GEdge[Any, Any]] =
    pagedEdgesF(ps => Task.deferFuture(database.edgesByFromAndKey.findByFromAndKey(fromId, key.iri, ps)))

  override def edgesByToId(toId: Long): Observable[graph.GEdge[Any, Any]] =
    pagedEdgesF(ps => Task.deferFuture(database.edgesByTo.findByTo(toId)))

  override def edgesByToIdAndKey(toId: Long, key: Property): Observable[graph.GEdge[Any, Any]] =
    pagedEdgesF(ps => Task.deferFuture(database.edgesByToAndKey.findByToAndKey(toId, key.iri, ps)))

  override def edgesByFromIdAndToId(fromId: Long, toId: Long): Observable[graph.GEdge[Any, Any]] =
    pagedEdgesF(ps => Task.deferFuture(database.edgesByFromAndTo.findByFromAndKeyAndTo(fromId, toId, ps)))

  override def edgesByFromIdAndKeyAndToId(fromId: Long, key: Property, toId: Long): Observable[graph.GEdge[Any, Any]] =
    pagedEdgesF(
      ps => Task.deferFuture(database.edgesByFromAndKeyAndTo.findByFromAndKeyAndTo(fromId, key.iri, toId, ps)))

//  override def edgesByKey(key: Property): Observable[graph._Edge[_, _]] =
//    pagedEdgesF(
//      () =>
//        Await
//          .result(database.edgesByKey.findById(ids), 5 seconds))

//  override def edgeByIri(iri: String): Observable[graph.GEdge[Any, Any]] =
//    for {
////      iriIds <- graph.valueStore.byValue(iri).map(_.id)
//      edges <- pagedEdgesF(ps => Task.deferFuture(database.edgesByIri.findByIris(iri :: Nil, ps)))
//    } yield edges

//  override def edgesByIri(iris: List[String]): Observable[graph.GEdge[Any, Any]] =
//    for {
////      iriIds <- Observable
////        .fromIterable(iris)
////        .flatMap(iri => graph.valueStore.byValue(iri).map(_.id)) //graph.valueStore.byValue(iri).map(_.id)
//      edges <- pagedEdgesF(ps => Task.deferFuture(database.edgesByIri.findByIris(iris, ps)))
//    } yield edges

  override def valueById(id: Long): Task[Option[graph.GValue[Any]]] =
    pagedValuesF(ps => Task.deferFuture(database.values.findById(id))).headOptionL

  override def valuesById(ids: List[Long]): Observable[graph.GValue[Any]] =
    pagedValuesF(ps => Task.deferFuture(database.values.findByIds(ids, ps)))

//  override def valueByIri(iri: String): Observable[graph.GValue[Any]] =
//    for {
////      iriIds <- graph.valueStore.byValue(iri).map(_.id)
//      values <- pagedValuesF(ps => Task.deferFuture(database.valuesByIri.findByIris(iri :: Nil, ps)))
//    } yield values
//
//  override def valuesByIri(iris: List[String]): Observable[graph.GValue[Any]] =
//    for {
////      iriIds <- Observable
////        .fromIterable(iris)
////        .flatMap(iri => graph.valueStore.byValue(iri).map(_.id)) //graph.valueStore.byValue(iri).map(_.id)
//      values <- pagedValuesF(ps => Task.deferFuture(database.valuesByIri.findByIris(iris, ps)))
//    } yield values

  override def valueByValue[T](value: T, dt: DataType[T]): Observable[graph.GValue[T]] =
    pagedValuesF(
      ps =>
        Task.deferFuture(
          database.valuesByValue.findByValue(encoder.fromData(value, dt)(ActiveContext()).json.toString(), ps)))
      .asInstanceOf[Observable[graph.GValue[T]]]

  override def valuesByValue[T](values: List[(T, DataType[T])]): Observable[graph.GValue[T]] =
    pagedValuesF(ps =>
      Task.deferFuture(database.valuesByValue.findByValues(values.map {
        case (value, dt) => encoder.fromData(value, dt)(ActiveContext()).json.toString()
      }, ps)))
      .asInstanceOf[Observable[graph.GValue[T]]]

  private def structureNodeToNode(node: graph._Node) =
    Node(
      node.id,
      Some(node.iri).filter(_.nonEmpty),
      node.outE(Property.default.typed.iriUrlString).headOption.map(e => e.id -> e.to.id),
      node.iris,
      node.outE(Property.default.typed.irisUrlString).sortBy(_.to.value).map(e => e.id -> e.to.id),
      node.labels.map(_.iri)
    )

  override def storeNodes(nodes: List[graph._Node]): Task[_] = loadBalancer.synchronized {
    val cnodes = nodes.distinct.map(structureNodeToNode)

    Observable
      .fromIterable(
        cnodes.map(database.nodes.store(_)).grouped(100).toStream.map(Batch.unlogged.add(_))
//          ++
//          cnodes
//            .filter(_.iri.nonEmpty)
//            .map(database.nodesByIri.store(_))
//            .grouped(100)
//            .toStream
//            .map(Batch.unlogged.add(_)) ++
//          cnodes
//            .filter(_.iris.nonEmpty)
//            .flatMap(n => n.iris.map(iri => n.copy(iri = Some(iri))))
//            .map(database.nodesByIris.store(_))
//            .grouped(100)
//            .toStream
//            .map(Batch.unlogged.add(_))
          map (r => (session, context) -> r.asInstanceOf[BatchQuery[com.outworkers.phantom.builder.ConsistencyBound]])
      )
      .consumeWith(loadBalancer)

  }

  private def structureEdgeToEdge(edge: graph._Edge[_, _]) = Edge(
    edge.id,
    Some(edge.iri).filter(_.nonEmpty),
    edge.outE(Property.default.typed.iriUrlString).headOption.map(e => e.id -> e.to.id),
    edge.iris,
    edge.outE(Property.default.typed.irisUrlString).sortBy(_.to.value).map(e => e.id -> e.to.id),
    edge.from.id,
    edge.from match {
      case n: structure.Node       => 0
      case e: structure.Edge[_, _] => 1
      case v: structure.Value[_]   => 2
    },
    edge.key.iri,
    edge.key.iris ++ edge.key.extendedClasses().flatMap(_.iris),
    edge.to.id,
    edge.to match {
      case n: structure.Node       => 0
      case e: structure.Edge[_, _] => 1
      case v: structure.Value[_]   => 2
    }
  )
  override def storeEdges(edges: List[graph._Edge[_, _]]): Task[_] = loadBalancer.synchronized {
    val cedges = edges.distinct.map(structureEdgeToEdge)
    Observable
      .fromIterable(
        cedges.map(database.edges.store(_)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
//          cedges
//            .filter(_.iri.nonEmpty)
//            .map(database.edgesByIri.store(_))
//            .grouped(100)
//            .toStream
//            .map(Batch.unlogged.add(_)) ++
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

  private def structureValueToValue(value: graph._Value[_]) = {
    val jip = encoder.fromData(value.value, value.label)(ActiveContext())
    Value(
      value.id,
      Some(value.iri).filter(_.nonEmpty),
      value.outE(Property.default.typed.iriUrlString).headOption.map(e => e.id -> e.to.id),
      value.iris,
      value.outE(Property.default.typed.irisUrlString).sortBy(_.to.value).map(e => e.id -> e.to.id),
      value.label.iri,
      Map("@context" -> jip.activeContext.asJson.getOrElse("{}".asJson)).asJson.toString,
      jip.json.toString
    )
  }

  override def storeValues(values: List[graph._Value[_]]): Task[_] = loadBalancer.synchronized {
    val cvalues = values.distinct.map(structureValueToValue)

    Observable
      .fromIterable(
        cvalues.map(database.values.store(_)).grouped(100).toStream.map(Batch.unlogged.add(_))
//          ++
//          cvalues
//            .filter(_.iri.nonEmpty)
//            .map(database.valuesByIri.store(_))
//            .grouped(100)
//            .toStream
//            .map(Batch.unlogged.add(_)) ++
//          cvalues.map(database.valuesByValue.store(_)).grouped(100).toStream.map(Batch.unlogged.add(_))
          map (r => (session, context) -> r.asInstanceOf[BatchQuery[com.outworkers.phantom.builder.ConsistencyBound]])
      )
      .consumeWith(loadBalancer)
  }

  def deleteNodes(nodes: List[graph._Node]): Task[_] = {
    val cnodes = nodes.map(structureNodeToNode) //ignores unretrievable nodes
    Observable
      .fromIterable(
        cnodes.map(n => database.nodes.delete(n.id)).grouped(100).toStream.map(Batch.unlogged.add(_))
//          ++
//          cnodes.map(n => database.nodesByIri.delete(n.id)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
//          cnodes
//            .flatMap(n => n.iris.map(iri => database.nodesByIris.delete(n.id)))
//            .grouped(100)
//            .toStream
//            .map(Batch.unlogged.add(_))
          map (r => (session, context) -> r.asInstanceOf[BatchQuery[com.outworkers.phantom.builder.ConsistencyBound]])
      )
      .consumeWith(loadBalancer)
  }
  def deleteEdges(edges: List[graph._Edge[_, _]]): Task[_] = {
    val cedges = edges.map(structureEdgeToEdge) //ignores unretrievable edges
    Observable
      .fromIterable(
        cedges.map(e => database.edges.delete(e.id)).grouped(100).toStream.map(Batch.unlogged.add(_)) ++
//          cedges
//            .filter(_.iri != 0l)
//            .map(e => database.edgesByIri.delete(e.id))
//            .grouped(100)
//            .toStream
//            .map(Batch.unlogged.add(_)) ++
          cedges
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
  def deleteValues(values: List[graph._Value[_]]): Task[_] = {
    val cvalues = values.map(structureValueToValue) //ignores unretrievable edges
    Observable
      .fromIterable(
        cvalues.map(v => database.values.delete(v.id)).grouped(100).toStream.map(Batch.unlogged.add(_))
          ++
//          cvalues
//            .filter(_.iri != 0l)
//            .map(v => database.valuesByIri.delete(v.id))
//            .grouped(100)
//            .toStream
//            .map(Batch.unlogged.add(_)) ++
            cvalues
              .map(v => database.valuesByValue.delete(v.id, v.value))
              .grouped(100)
              .toStream
              .map(Batch.unlogged.add(_))
          map (r => (session, context) -> r.asInstanceOf[BatchQuery[com.outworkers.phantom.builder.ConsistencyBound]])
      )
      .consumeWith(loadBalancer)
  }

  override def nodes: Observable[graph.GNode] =
    pagedNodesF(ps => Task.deferFuture(database.nodes.select.paginateRecord(ps))) //fetchRecord()))

  def nodeCount(): Task[Long] = Task.deferFuture(database.nodes.select.count().one()).map {
    case Some(count) => count
    case None        => 0L
  }

  override def edges: Observable[graph.GEdge[Any, Any]] =
    pagedEdgesF(ps => Task.deferFuture(database.edges.select.paginateRecord(ps)))

  def edgeCount(): Task[Long] = Task.deferFuture(database.edges.select.count().one()).map {
    case Some(count) => count
    case None        => 0L
  }

  override def values: Observable[graph.GValue[Any]] =
    pagedValuesF(ps => Task.deferFuture(database.values.select.paginateRecord(ps)))

  def valueCount(): Task[Long] = Task.deferFuture(database.values.select.count().one()).map {
    case Some(count) => count
    case None        => 0L
  }

  lazy val init: Task[Unit] =
    (for {
      _ <- Task.gatherUnordered(
        Seq(
          Task.deferFuture(database.nodes.create.ifNotExists().future()),
//          Task.deferFuture(database.nodesByIri.create.ifNotExists().future()),
//          Task.deferFuture(database.nodesByIris.create.ifNotExists().future()),
          Task.deferFuture(database.edges.create.ifNotExists().future()),
//          Task.deferFuture(database.edgesByIri.create.ifNotExists().future()),
          Task.deferFuture(database.edgesByFrom.create.ifNotExists().future()),
          Task.deferFuture(database.edgesByFromAndKey.create.ifNotExists().future()),
          Task.deferFuture(database.edgesByFromAndKeyAndTo.create.ifNotExists().future()),
          Task.deferFuture(database.edgesByFromAndTo.create.ifNotExists().future()),
          Task.deferFuture(database.edgesByToAndKey.create.ifNotExists().future()),
          Task.deferFuture(database.edgesByTo.create.ifNotExists().future()),
          Task.deferFuture(database.values.create.ifNotExists().future()),
//          Task.deferFuture(database.valuesByIri.create.ifNotExists().future()),
          Task.deferFuture(database.valuesByValue.create.ifNotExists().future())
        ))
    } yield ()).memoizeOnSuccess

  def persist(): Task[Unit] = Task.unit

  def purge: Task[Unit] =
    for {
//      _ <- Task.deferFuture(database.dropAsync()).onErrorHandle { f =>
//        f.printStackTrace(); throw f
//      }
//      _ <- Task.deferFuture(database.createAsync()).onErrorHandle { f =>
//        f.printStackTrace(); throw f
//      }
      _ <- Task.deferFuture(database.truncateAsync()).onErrorHandle { f =>
        f.printStackTrace(); throw f
      }
    } yield ()

  def close(): Task[Unit] = Task {
    database.shutdown()
  }

}
