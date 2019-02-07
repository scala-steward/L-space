package lspace.lgraph.provider.file

import argonaut._
import Argonaut._
import java.io.PrintWriter
import java.time.{Instant, LocalDate, LocalTime}

import cats.effect.Resource
import lspace.codec.exception.FromJsonException
import lspace.lgraph._
import lspace.lgraph.store.StoreManager
import lspace.librarian.datatype._
import lspace.librarian.structure.{Edge, Node, Property}
import lspace.parse.{ActiveContext, JsonObjectInProgress}
import monix.eval.Task
import monix.execution.{Cancelable, CancelableFuture}
import monix.reactive.Observable

import scala.io.BufferedSource

object FileStoreManager {

  def apply[G <: LGraph](graph: G, path: String): FileStoreManager[G] = new FileStoreManager[G](graph, path) {}
}

/**
  * This manager stores all resources to a filesystem. It builds a the complete graph in memory on initialization
  * and persists (async) on any commits to the graph.
  * @param graph
  * @tparam G
  */
class FileStoreManager[G <: LGraph](override val graph: G, path: String) extends StoreManager(graph) {
  private val directory = new java.io.File(path)
  directory.mkdirs
  if (!directory.isDirectory) throw new Exception(s"storage path ${directory.getAbsolutePath} is not a directory")

  private def readFile(filename: String): Resource[Task, BufferedSource] = {
    import java.io._
    Resource.make {
      Task(try { scala.io.Source.fromFile(filename) } catch {
        case e: FileNotFoundException =>
          new BufferedSource(new InputStream {
            override def read(): Int = {
              -1 // end of stream
            }
          }) //from java 11 this can be replaced with InputStream.nullInputStream()
      })
    } { in =>
      Task(in.close())
    }
  }
  private def writeFile(filename: String): Resource[Task, PrintWriter] = {
    import java.io._
    Resource.make {
      Task {
//        println(s"making file ${filename}")
        val file = new File(filename)
        if (!file.exists) file.getParentFile.mkdirs
        new PrintWriter(new BufferedWriter(new FileWriter(filename)))
      }
    } { in =>
      Task(in.close())
    }
  }
  private object graphfiles {
    object read {
      private def subscribeFile(name: String) = readFile(path.stripSuffix("/") + "/" + name)
      val nodes                               = subscribeFile("nodes.303")
      val literalEdges                        = subscribeFile("literal_edges.303")
      val structuredEdges                     = subscribeFile("structured_edges.303")
      val literals                            = subscribeFile("literals.303")
      val structures                          = subscribeFile("structures.303")
      val context = new {
        val nodes           = subscribeFile("nodes.context.303")
        val literalEdges    = subscribeFile("literal_edges.context.303")
        val structuredEdges = subscribeFile("structured_edges.context.303")
        val literals        = subscribeFile("literals.context.303")
        val structures      = subscribeFile("structures.context.303")
      }
    }
    object write {
      private def publishFile(name: String) = writeFile(path.stripSuffix("/") + "/" + name)
      val nodes                             = publishFile("nodes.303")
      val literalEdges                      = publishFile("literal_edges.303")
      val structuredEdges                   = publishFile("structured_edges.303")
      val literals                          = publishFile("literals.303")
      val structures                        = publishFile("structures.303")
      object context {
        val nodes           = publishFile("nodes.context.303")
        val literalEdges    = publishFile("literal_edges.context.303")
        val structuredEdges = publishFile("structured_edges.context.303")
        val literals        = publishFile("literals.context.303")
        val structures      = publishFile("structures.context.303")
      }
    }
  }

  override def nodeById(id: Long): Option[graph._Node with LNode] = None

  override def nodesById(ids: List[Long]): Stream[graph._Node with LNode] = Stream()

  override def nodeByIri(iri: String): Stream[graph._Node with LNode] = Stream()

  override def nodesByIri(iri: List[String]): Stream[graph._Node with LNode] = Stream()

  override def edgeById(id: Long): Option[graph._Edge[Any, Any] with LEdge[Any, Any]] = None

  override def edgesById(ids: List[Long]): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def edgesByFromId(fromId: Long): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def edgesByFromIdAndKey(fromId: Long, key: Property): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] =
    Stream()

  override def edgesByToId(toId: Long): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def edgesByToIdAndKey(toId: Long, key: Property): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] =
    Stream()

  override def edgesByFromIdAndToId(fromId: Long, toId: Long): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] =
    Stream()

  override def edgesByFromIdAndKeyAndToId(fromId: Long,
                                          key: Property,
                                          toId: Long): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def edgeByIri(iri: String): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def edgesByIri(iri: List[String]): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def valueById(id: Long): Option[graph._Value[Any] with LValue[Any]] = None

  override def valuesById(ids: List[Long]): Stream[graph._Value[Any] with LValue[Any]] = Stream()

  override def valueByIri(iri: String): Stream[graph._Value[Any] with LValue[Any]] = Stream()

  override def valuesByIri(iri: List[String]): Stream[graph._Value[Any] with LValue[Any]] = Stream()

  override def valueByValue[T](value: T, dt: DataType[T]): Stream[graph._Value[T] with LValue[T]] = Stream()

  override def valuesByValue[T](values: List[(T, DataType[T])]): Stream[graph._Value[T] with LValue[T]] = Stream()

  override def storeNodes(nodes: List[graph._Node with LNode]): Task[_] = Task {}

  override def storeEdges(
      edges: List[(graph._Edge[_$1, _$2] with LEdge[_$1, _$2]) forSome { type _$1; type _$2 }]): Task[_] =
    Task {}

  override def storeValues(values: List[(graph._Value[_$1] with LValue[_$1]) forSome { type _$1 }]): Task[_] =
    Task {}

  override def deleteNodes(nodes: List[graph._Node with LNode]): Task[_] =
    Task {}

  override def deleteEdges(
      edges: List[(graph._Edge[_$1, _$2] with LEdge[_$1, _$2]) forSome { type _$1; type _$2 }]): Task[_] =
    Task {}

  override def deleteValues(values: List[(graph._Value[_$1] with LValue[_$1]) forSome { type _$1 }]): Task[_] =
    Task {}

  override val nodes: Stream[graph._Node with LNode] = Stream()

  override val edges: Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override val values: Stream[graph._Value[Any] with LValue[Any]] = Stream()

  override def nodeCount(): Long = graph.nodes().size

  override def edgeCount(): Long = graph.edges().size

  override def valueCount(): Long = graph.values().size

  import argonaut._
  import Argonaut._
  private val encoder = EncodeLDFS()
  private val decoder = DecodeLDFS(graph)

  private def parse(buf: BufferedSource): Observable[Json] = {
    val it = buf.getLines()
    Observable.fromIterator(Task(it)).mapParallelUnordered(20) { line =>
      Parse.parse(line) match {
        case Left(issues) => Task.raiseError(FromJsonException(issues))
        case Right(json)  => Task(json)
      }
    }
  }

  private def parseContext(buf: BufferedSource): Task[decoder.AC] = {
    val it = buf.getLines()
    if (it.hasNext) Parse.parse(it.next()) match {
      case Left(issues) => Task.raiseError(FromJsonException(issues))
      case Right(json) =>
        json.obj
          .map(obj => decoder.extractContext(obj.toMap)(ActiveContext()))
          .getOrElse(Task(ActiveContext()))
    } else Task(ActiveContext())
  }

  lazy val init: CancelableFuture[Unit] = {
    readLiterals
      .flatMap { u =>
        readNodes
          .flatMap(readLiteralEdges)
          .flatMap(readStructures)
          .flatMap(readStructuredEdges)
      }
      .foreachL { f =>
        Unit
      }
      .runToFuture(monix.execution.Scheduler.global)
  }

  private def readLiterals: Task[Unit] = {
    graphfiles.read.context.literals
      .use(parseContext)
      .flatMap { implicit context =>
        graphfiles.read.literals.use { buf =>
          parse(buf).mapEval { json =>
            json.obj match {
              case Some(obj) =>
                Task {
                  obj.toList.map {
                    case (key, value) =>
                      val values = value.array.get
                      val label  = graph.ns.datatypes.get(key).get
                      (label match {
                        case TextType.datatype =>
                          values.map(_.string.getOrElse(throw FromJsonException("cannot parse text")))
                        case tpe: NumericType[_] =>
                          tpe match {
                            case DoubleType.datatype =>
                              values.map(
                                _.number
                                  .flatMap(_.toDouble)
                                  .getOrElse(throw FromJsonException("cannot parse double")))
                            case LongType.datatype =>
                              values.map(
                                _.string
                                  .map(_.toLong)
                                  .getOrElse(throw FromJsonException("cannot parse long")))
                            case IntType.datatype =>
                              values.map(
                                _.number
                                  .flatMap(_.toInt)
                                  .getOrElse(throw FromJsonException("cannot parse int")))
                          }
                        case tpe: CalendarType[_] =>
                          tpe match {
                            case DateTimeType.datatype =>
                              values.map(
                                _.string
                                  .map(Instant.parse(_))
                                  .getOrElse(throw FromJsonException("cannot parse datetime")))
                            case LocalDateType.datatype =>
                              values.map(
                                _.string
                                  .map(LocalDate.parse(_))
                                  .getOrElse(throw FromJsonException("cannot parse date")))
                            case LocalTimeType.datatype =>
                              values.map(
                                _.string
                                  .map(LocalTime.parse(_))
                                  .getOrElse(throw FromJsonException("cannot parse time")))
                          }
                        //        case tpe: ColorType[_] =>
                        case BoolType.datatype =>
                          values.map(_.bool.getOrElse(throw FromJsonException("cannot parse bool")))
                      }).map(graph.values.create(_, label))
                  }
                }
              case None =>
                Task.raiseError(FromJsonException("literals-line should be an object"))
            }
          }.completedL
        }
      }
  }
  private def readNodes: Task[IdMaps] = {
    graphfiles.read.context.nodes
      .use(parseContext)
      .flatMap { implicit context =>
        graphfiles.read.nodes
          .use { buf =>
            (Observable(IdMaps()) ++ parse(buf)
              .mapEval { json =>
                json.array
                  .map {
                    case List(keys, value) =>
                      val ids = value.array.getOrElse(throw FromJsonException("values not an array"))
                      val labels = keys.array
                        .map(
                          _.map(
                            _.string
                              .map(context.expandIri)
                              .getOrElse(throw FromJsonException("key not a string"))
                          ).map(graph.ns.ontologies
                            .get(_)
                            .getOrElse(throw FromJsonException("unknown ontology"))))
                        .getOrElse(throw FromJsonException("keys not an array"))
                      Observable
                        .fromIterable(
                          ids.map(_.number.flatMap(_.toLong).getOrElse(throw FromJsonException("id not a long"))))
                        .mapParallelUnordered(20) { id =>
                          Task {
                            id -> graph.nodes.create(labels: _*).id
                          }
                        }
                        .toListL
                    case _ =>
                      Task.raiseError(FromJsonException("nodes-line should be an [[label-ref*][id*]]"))
                  }
                  .getOrElse(Task.raiseError(FromJsonException("nodes-line should be an [[label-ref*][id*]]")))
              }
              .reduce(_ ++ _)
              .map(_.toMap)
              .map(IdMaps(_))).toListL
              .map(_.reduce(_ ++ _))
          }
      }
  }

  private def readLiteralEdges(idMaps: IdMaps): Task[IdMaps] = {
//    println(s"ledges ${graph.iri}")
    val jsonld = EncodeLDFS(idMaps)
    graphfiles.read.context.literalEdges
      .use(parseContext)
      .flatMap { implicit context =>
        graphfiles.read.literalEdges
          .use { buf =>
            parse(buf)
              .flatMap { json => //line of complete json
                json.obj
                  .map { obj =>
                    Observable.fromIterable(obj.toList).flatMap {
                      case (key, value) =>
                        graph.ns.properties
                          .get(context.expandIri(key))
                          .map { property =>
                            value.array
                              .map {
                                case List(byNode, byValue) =>
                                  Observable
                                    .fromIterable(byNode.array.getOrElse(throw FromJsonException("array expected")))
                                    .mapParallelUnordered(20)(_.array
                                      .map {
                                        case List(from, to) =>
                                          for {
                                            fromNode <- decoder
                                              .tryNodeRef(from)
                                              .getOrElse(throw FromJsonException("cannot parse noderef"))
                                            fromResource <- decoder.toResource(to, property.range.headOption)
                                          } yield {
                                            fromNode --- property --> fromResource
                                          }
                                        case _ => throw FromJsonException(s"expected an array of size 2")
                                      }
                                      .getOrElse(throw FromJsonException("array expected")))
                                //                              byValue.array.map(_.map(
                                //                                _.number.flatMap(_.toLong).getOrElse(throw FromJsonException("from node id expected"))))
                                case _ => throw FromJsonException(s"expected an array of size 2")
                              }
                              .getOrElse(throw FromJsonException(s"expected an array"))

                          }
                          .getOrElse(throw FromJsonException(s"$key property could not be build/fetched"))
                    }
                  }
                  .getOrElse(Observable.raiseError(FromJsonException("line should be a json-object")))
              }
              .completedL
              .map { u =>
                idMaps
              }
          }
      }
  }

  private def readStructuredEdges(idMaps: IdMaps): Task[IdMaps] = {
//    println("sedges")
    val jsonld = EncodeLDFS(idMaps)
    graphfiles.read.context.structuredEdges
      .use(parseContext)
      .flatMap { implicit context =>
        graphfiles.read.structuredEdges
          .use { buf =>
            parse(buf)
              .flatMap { json =>
                json.obj
                  .map { obj =>
                    Observable.fromIterable(obj.toList).flatMap {
                      case (key, value) =>
                        graph.ns.properties
                          .get(context.expandIri(key))
                          .map { property =>
                            value.array
                              .map(_.toList)
                              .map {
                                case List(byNode, byEdge, byValue) =>
                                  Observable
                                    .fromIterable(byNode.array.getOrElse(throw FromJsonException("array expected")))
                                    .mapParallelUnordered(20)(_.array
                                      .map {
                                        case List(from, to) =>
                                          for {
                                            fromNode <- decoder
                                              .tryNodeRef(from)
                                              .getOrElse(throw FromJsonException("cannot parse noderef"))
                                            fromResource <- {
                                              decoder.toResource(to, property.range.headOption)
                                            }
                                          } yield {
                                            fromNode --- property --> fromResource
                                          }
                                        case _ => throw FromJsonException(s"expected an array of size 2")
                                      }
                                      .getOrElse(throw FromJsonException("array expected")))
                                //                              byValue.array.map(_.map(
                                //                                _.number.flatMap(_.toLong).getOrElse(throw FromJsonException("from node id expected"))))
                                case _ => throw FromJsonException(s"expected an array of size 2")
                              }
                              .getOrElse(throw FromJsonException(s"expected an array"))

                          }
                          .getOrElse(throw FromJsonException(s"$key property could not be build/fetched"))
                    }
                  }
                  .getOrElse(Observable.raiseError(FromJsonException("line should be a json-object")))
              }
              .onErrorHandle { case e => scribe.error(e.getMessage); throw e }
              .completedL
              .map { u =>
                idMaps
              }
          }
      }
  }

  private def readStructures(idMaps: IdMaps): Task[IdMaps] = {
//    println("s")
    val jsonld = EncodeLDFS(idMaps)
    graphfiles.read.context.structures
      .use(parseContext)
      .flatMap { implicit context =>
        graphfiles.read.structures
          .use { buf =>
            parse(buf)
              .mapEval { json =>
                json.array
                  .map {
                    case List(id, label, value) =>
                      (for {
                        longId <- id.number.flatMap(_.toLong)
                        datatype <- label.string
                          .map(context.expandIri)
                          .flatMap { iri =>
                            decoder.iriToClassType(iri)
                          }
                          .collect { case dt: DataType[_] => dt }
                      } yield {
                        decoder.toValue(value, datatype).map(_.id).map(longId -> _)
                      }).getOrElse(throw FromJsonException("id not a long, label unknown or value not parsable"))
                    case _ =>
                      Task.raiseError(FromJsonException("nodes-line should be an [[label-ref*][id*]]"))
                  }
                  .getOrElse(Task.raiseError(FromJsonException("nodes-line should be an [[label-ref*][id*]]")))
              }
              .toListL
              .map(edgeIds => IdMaps(edgeIds = edgeIds.toMap))
              .map { idMaps ++ _ }
          }
      }
  }

  override def persist: CancelableFuture[Unit] = {
    val encoder = EncodeLDFS()
    import argonaut._
    import Argonaut._
//    println(s"persisting ${graph.iri} to $path")

    Task
      .gatherUnordered(
        Seq(
          graphfiles.write.literals.use { f =>
            Task {
              graph
                .values()
                .flatMap(_.hasLabel(LiteralType.datatype))
                .groupBy(_.label)
                .mapValues(_.map(_.value))
                .foreach {
                  case (label, values) =>
                    implicit val ac = ActiveContext()
                    val jsons = label match {
                      case label: TextType[_] => values.map(encoder.fromText(_, label).json)
                      case label: NumericType[_] =>
                        label match {
                          case label: IntType[_]    => values.map(encoder.fromInt(_, label).json)
                          case label: DoubleType[_] => values.map(encoder.fromDouble(_, label).json)
                          case label: LongType[_]   => values.map(encoder.fromLong(_, label).json)
                        }
                      case label: CalendarType[_] =>
                        label match {
                          case label: DateTimeType[_]  => values.map(encoder.fromDateTime(_, label).json)
                          case label: LocalDateType[_] => values.map(encoder.fromDate(_, label).json)
                          case label: LocalTimeType[_] => values.map(encoder.fromTime(_, label).json)
                        }
                      //            case label: BoolType[_] => values.map(jsonld.encode.from(_))
                    }
                    jsons.sliding(50, 50).foreach { jsons =>
                      f.println(Json.jObject(JsonObject.single(label.iri, jsons.toList.asJson)).toString())
                    }
                }
            }
          },
          graphfiles.write.nodes
            .use { f =>
//              println(s"persisting ${graph.nodes.count()} nodes in $path")
              Task {
                graph
                  .nodes()
                  .groupBy(_.labels.toSet)
                  .foldLeft[decoder.AC](ActiveContext()) {
                    case (ac, (labels, nodes)) =>
                      val (newAc, cLabels) = labels.foldLeft(ac -> List[String]()) {
                        case ((ac, labels), label) =>
                          val (cIri, newAc) = ac.compactIri(label)
                          newAc -> (cIri :: labels)
                      }
                      val json = List(cLabels.map(_.asJson).asJson, nodes.map(_.id.asJson).asJson).asJson
                      f.println(json.toString())
                      newAc
                  }
              }
            }
            .flatMap { ac =>
              graphfiles.write.context.nodes.use { f =>
                Task {
                  import JsonObjectInProgress._
                  f.println(
                    lspace.codec
                      .JsonObjectInProgress[Json, JsonObject](JsonObject.empty)(ac)
                      .withContextAsJson
                      .toString())
                }
              }
            },
          graphfiles.write.literalEdges
            .use { f =>
              Task {
                graph
                  .edges()
                  .filter(e =>
                    (e.from.isInstanceOf[Node] || e.from.hasLabel(LiteralType.datatype).isDefined) && (e.to
                      .isInstanceOf[Node] || e.to.hasLabel(LiteralType.datatype).isDefined))
                  .groupBy(_.key)
                  .foldLeft[decoder.AC](ActiveContext()) {
                    case (ac, (key, edges)) =>
                      val (cKeyIri, _ac)            = ac.compactIri(key)
                      val (byFromNode, byFromValue) = edges.partition(_.from.isInstanceOf[Node])
                      val (newAc, fromNodeJsons) = byFromNode.foldLeft(_ac -> List[Json]()) {
                        case ((ac, jsons), edge) =>
                          val jip = encoder.fromAny(edge.to, edge.key.range.headOption)(ac)
                          jip.activeContext -> (List(edge.from.id.asJson, jip.json).asJson :: jsons)
                      }
                      val (newAc2, fromLiteralJsons) = byFromValue.foldLeft(newAc -> List[Json]()) {
                        case ((ac, jsons), edge) =>
                          val jip  = encoder.fromAny(edge.to, edge.key.range.headOption)(ac)
                          val jip2 = encoder.fromAny(edge.from)(jip.activeContext)
                          jip2.activeContext -> (List(jip2.json, jip.json).asJson :: jsons)
                      }
                      fromNodeJsons.sliding(50, 50).zipAll(fromLiteralJsons.sliding(50, 50), List(), List()).foreach {
                        case (fromNodes, fromLiterals) =>
                          val json = jObject(JsonObject.single(
                            cKeyIri,
                            List(fromNodes.asJson, fromLiterals.asJson).asJson
                          ))
                          f.println(json.toString)
                      }

                      newAc2
                  }
              }
            }
            .flatMap { ac =>
              graphfiles.write.context.literalEdges.use { f =>
                Task {
                  import lspace.parse.JsonObjectInProgress._
                  f.println(lspace.codec.JsonObjectInProgress(JsonObject.empty)(ac).withContextAsJson.toString())
                }
              }
            },
          graphfiles.write.structuredEdges
            .use { f =>
              Task {
                graph
                  .edges()
                  .filterNot(e =>
                    (e.from.isInstanceOf[Node] || e.from.hasLabel(LiteralType.datatype).isDefined) && (e.to
                      .isInstanceOf[Node] || e.to.hasLabel(LiteralType.datatype).isDefined))
                  .groupBy(_.key)
                  .foldLeft[decoder.AC](ActiveContext()) {
                    case (ac, (key, edges)) =>
                      val (cKeyIri, _ac)                  = ac.compactIri(key)
                      val (byFromNode, byFromEdgeOrValue) = edges.partition(_.from.isInstanceOf[Node])
                      val (newAc, fromNodeJsons) = byFromNode.foldLeft(_ac -> List[Json]()) {
                        case ((ac, jsons), edge) =>
                          val jip = encoder.fromAny(edge.to, edge.key.range.headOption)(ac)
                          jip.activeContext -> (List(edge.from.id.asJson, jip.json).asJson :: jsons)
                      }
                      val (byFromEdge, byFromValue) = byFromEdgeOrValue.partition(_.from.isInstanceOf[Edge[_, _]])
                      val (newAc2, fromEdgeJsons) = byFromEdge.foldLeft(newAc -> List[Json]()) {
                        case ((ac, jsons), edge) =>
                          val jip  = encoder.fromAny(edge.to, edge.key.range.headOption)(ac)
                          val jip2 = encoder.fromAny(edge.from)(jip.activeContext)
                          jip2.activeContext -> (List(jip2.json, jip.json).asJson :: jsons)
                      }
                      val (newAc3, fromLiteralJsons) = byFromValue.foldLeft(newAc2 -> List[Json]()) {
                        case ((ac, jsons), edge) =>
                          val jip  = encoder.fromAny(edge.to, edge.key.range.headOption)(ac)
                          val jip2 = encoder.fromAny(edge.from)(jip.activeContext)
                          jip2.activeContext -> (List(jip2.json, jip.json).asJson :: jsons)
                      }

                      val json = jObject(
                        JsonObject.single(
                          cKeyIri,
                          List(fromNodeJsons.asJson, fromEdgeJsons.asJson, fromLiteralJsons.asJson).asJson
                        ))
                      f.println(json.toString)

                      newAc3
                  }
              }
            }
            .flatMap { ac =>
              graphfiles.write.context.structuredEdges.use { f =>
                Task {
                  import lspace.parse.JsonObjectInProgress._
                  f.println(
                    lspace.codec
                      .JsonObjectInProgress(JsonObject.empty)(ac)
                      .withContextAsJson
                      .toString())
                }
              }
            },
          graphfiles.write.structures
            .use { f =>
              /*graph.values().flatMap(_.hasLabel(StructuredType.datatype)).groupBy(_.label).foreach {
          case (label, values) =>
            val jsons = label match {
              case label: CollectionType[_] =>
              case label: TupleType[_] => label match {
                case label: Tuple2Type[_,_] =>
                case label: Tuple3Type[_,_,_] =>
                case label: Tuple4Type[_,_,_,_] =>
              }
              case label: GeometricType[_] => label match {
                case label: GeopointType[_] => encoder.fromGeometric(_, label)
//                case label: GeoMultipointType[_] =>
//                case label: GeoLineType[_] =>
//                case label: GeoMultiLineType[_] =>
                case label: GeoPolygonType[_] =>
//                case label: GeoMultiPolygonType[_] =>
//                case label: GeoMultiGeometryType[_] =>
              }
//              case label: QuantityType[_] =>
//              case label: ColorType[_] =>
            }
        }*/
              Task {
                graph
                  .values()
                  .flatMap(_.hasLabel(StructuredType.datatype))
                  .foldLeft[encoder.AC](ActiveContext()) {
                    case (ac, v) =>
                      val jip            = encoder.fromStructured(v.value, v.label.asInstanceOf[StructuredType[_]])(ac)
                      val (label, newAc) = jip.activeContext.compactIri(v.label)
                      f.println(List(v.id.asJson, label.asJson, jip.json).asJson)
                      newAc
                  }
              }
            }
            .flatMap { ac =>
              graphfiles.write.context.structures.use { f =>
                Task {
                  import lspace.parse.JsonObjectInProgress._
                  f.println(
                    lspace.codec
                      .JsonObjectInProgress[Json, JsonObject](JsonObject.empty)(ac)
                      .withContextAsJson
                      .toString())
                }
              }
            }
        ))
      .foreachL(f => {})
      .runToFuture(monix.execution.Scheduler.global)
  }

  /**
    * finishes write-queue(s) and closes connection
    */
  override def close(): CancelableFuture[Unit] = CancelableFuture.unit //persist
}
