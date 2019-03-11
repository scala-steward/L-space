package lspace.lgraph.provider.file

import java.io.PrintWriter
import java.time.{Instant, LocalDate, LocalTime}

import cats.effect.{Resource => CResource}
import lspace.codec._
import lspace.codec.exception.FromJsonException
import lspace.lgraph._
import lspace.lgraph.store.StoreManager
import lspace.datatype._
import lspace.structure._
import monix.eval.Task
import monix.execution.{Cancelable, CancelableFuture}
import monix.reactive.Observable

import scala.io.BufferedSource

object FileStoreManager {

  def apply[G <: LGraph, Json](graph: G, path: String)(implicit encoder: NativeTypeEncoder.Aux[Json],
                                                       decoder: NativeTypeDecoder.Aux[Json]) =
    new FileStoreManager(graph, path) {}
}

/**
  * This manager stores all resources to a filesystem. It builds a the complete graph in memory on initialization
  * and persists (async) on any commits to the graph.
  * @param graph
  * @tparam G
  */
class FileStoreManager[G <: LGraph, Json](override val graph: G, path: String)(
    implicit baseEncoder: NativeTypeEncoder.Aux[Json],
    baseDecoder: NativeTypeDecoder.Aux[Json])
    extends StoreManager(graph) {

  val encoder: EncodeLDFS[Json] = EncodeLDFS()
  val decoder: DecodeLDFS[Json] = DecodeLDFS(graph)
  import decoder.{baseDecoder => _, Json => _, _}
  import encoder.{baseEncoder => _, Json => _, _}

  private val directory = new java.io.File(path)
  directory.mkdirs
  if (!directory.isDirectory) throw new Exception(s"storage path ${directory.getAbsolutePath} is not a directory")

  private def readFile(filename: String): CResource[Task, BufferedSource] = {
    import java.io._
    CResource.make {
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
  private def writeFile(filename: String): CResource[Task, PrintWriter] = {
    import java.io._
    CResource.make {
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

//  import argonaut._
//  import Argonaut._
//  implicit private val encoder: lspace.codec.Encoder[Json] = EncodeLDFS()
//  private val decoder                                      = DecodeLDFS(graph)

  private def parse(buf: BufferedSource): Observable[Json] = {
    val it = buf.getLines()
    Observable.fromIterator(Task(it)).mapParallelUnordered(20) { line =>
      baseDecoder.parse(line).onErrorHandle { f =>
        scribe.error("error parsing data: " + f.getMessage)
        baseEncoder.encode("")
      }
    }
  }

  private def parseContext(buf: BufferedSource): Task[decoder.AC] = {
    val it = buf.getLines()
    if (it.hasNext) {
      val raw = it.next()
      decoder
        .parse(raw)
        .onErrorHandle { f =>
          scribe.error(f.getMessage + s" ${raw}")
          encoder.textToJson("{}")
        }
        .flatMap { json =>
          import decoder._
          Map(lspace.NS.types.`@context` -> json).extractContext(ActiveContext())
        }
    } //TODO: parse remote context? List of contexts?
    else Task(ActiveContext())
  }

  lazy val init: Task[Unit] = {
    Task.delay(scribe.info(s"reading from files ${graph.iri}")).flatMap { f =>
      readLiterals
        .flatMap { u =>
          readNodes
            .flatMap(readLiteralEdges)
            .flatMap(readStructures)
            .flatMap(readStructuredEdges)
        }
        .onErrorHandle { f =>
          f.printStackTrace(); throw f
        }
        .foreachL { f =>
          Unit
        }
    }
  }

  private def readLiterals: Task[Unit] = { scribe.info(s"read literals ${graph.iri}")
    graphfiles.read.context.literals
      .use(parseContext)
      .flatMap { implicit context =>
        graphfiles.read.literals.use { buf =>
          parse(buf).mapEval { json =>
            json.obj match {
              case Some(obj) =>
                Task.gatherUnordered(obj.toList.map {
                  case (key, value) =>
                    val values = value.list.get
                    graph.ns.datatypes.get(key).map {
                      labelOption =>
                        labelOption
                          .map {
                            label =>
                              (
                                label match {
                                  case TextType.datatype =>
                                    values.map(_.string.getOrElse(throw FromJsonException("cannot parse text")))
                                  case tpe: NumericType[_] =>
                                    tpe match {
                                      case DoubleType.datatype =>
                                        values.map(_.double
                                          .getOrElse(throw FromJsonException("cannot parse double")))
                                      case LongType.datatype =>
                                        values.map(_.long
                                          .getOrElse(throw FromJsonException("cannot parse long")))
                                      case IntType.datatype =>
                                        values.map(_.int
                                          .getOrElse(throw FromJsonException("cannot parse int")))
                                    }
                                  case tpe: CalendarType[_] =>
                                    tpe match {
                                      case DateTimeType.datatype =>
                                        values.map(_.string
                                          .map(Instant.parse(_))
                                          .getOrElse(throw FromJsonException("cannot parse datetime")))
                                      case LocalDateType.datatype =>
                                        values.map(_.string
                                          .map(LocalDate.parse(_))
                                          .getOrElse(throw FromJsonException("cannot parse date")))
                                      case LocalTimeType.datatype =>
                                        values.map(_.string
                                          .map(LocalTime.parse(_))
                                          .getOrElse(throw FromJsonException("cannot parse time")))
                                    }
                                  //        case tpe: ColorType[_] =>
                                  case BoolType.datatype =>
                                    values.map(_.boolean.getOrElse(throw FromJsonException("cannot parse bool")))
                                }
                              ).map(graph.values.create(_, label))
                          }
                          .getOrElse(throw FromJsonException(s"unknown datatype ${key}"))
                    }
                })
              case None =>
                Task.raiseError(FromJsonException("literals-line should be an object"))
            }
          }.completedL
        }
      }
  }
  private def readNodes: Task[IdMaps] = { scribe.info(s"read nodes ${graph.iri}")
//    println(s"nodes ${graph.iri}")
    graphfiles.read.context.nodes
      .use(parseContext)
      .flatMap { implicit context =>
        graphfiles.read.nodes
          .use { buf =>
            (Observable(IdMaps()) ++ parse(buf)
              .mapEval { json =>
                json.list
                  .map {
                    case List(keys, value) =>
                      val ids = value.list.getOrElse(throw FromJsonException("values not an array"))
                      val labels = keys.list
                        .map(
                          _.map(
                            _.string
                              .map(context.expandIri)
                              .getOrElse(throw FromJsonException("key not a string"))
                          ).map(iri =>
                            graph.ns.ontologies
                              .get(iri)
                              .map(_.getOrElse(throw FromJsonException(s"unknown ontology $iri")))))
                        .getOrElse(throw FromJsonException("keys not an array"))

                      Observable
                        .fromIterable(ids.map(_.long.getOrElse(throw FromJsonException("id not a long"))))
                        .mapParallelUnordered(20) { id =>
                          Task.gatherUnordered(labels).map { labels =>
                            id -> graph.nodes.create(labels: _*).id
                          }
                        }
                        .toListL
                    case _ =>
                      Task.raiseError(FromJsonException("nodes-line should be an [[label-ref*][id*]]"))
                  }
                  .getOrElse(Task.raiseError(FromJsonException("nodes-line should be an [[label-ref*][id*]]")))
              }
              .append(List()) //TEMP-FIX: https://github.com/monix/monix/issues/832 reducing on an observable with only one item results in an empty stream
              .reduce(_ ++ _)
              .map(_.toMap)
              .map(IdMaps(_))).toListL
              .map(_.reduce(_ ++ _))
          }
      }
  }

  private def readLiteralEdges(idMaps: IdMaps): Task[IdMaps] = {
    scribe.info(s"read literals edges ${graph.iri}")
    val decoder = DecodeLDFS(graph, idMaps)
    graphfiles.read.context.literalEdges
      .use(parseContext)
      .flatMap { implicit context =>
        graphfiles.read.literalEdges
          .use { buf =>
            parse(buf)
              .flatMap { json => //line of complete json
                json.list
                  .map {
                    case List(keyJson, fromLabelsJson, toLabelsJson, value) =>
                      Observable
                        .fromTask(
                          for {
                            property <- graph.ns.properties
                              .get(context.expandIri(keyJson.string.get))
                              .flatMap(_.map(Task.now).getOrElse(
                                Task.raiseError(FromJsonException(s"$keyJson property could not be build/fetched"))))
                            fromLabels <- Task.gather(fromLabelsJson.list.toList.flatMap(_.flatMap(_.string).map(context.expandIri).map(iri => graph.ns.classtypes
                              .get(context.expandIri(iri))
                              .flatMap(_.map(Task.now).getOrElse(
                                Task.raiseError(FromJsonException(s"$fromLabelsJson classtypes could not be build/fetched")))))))
                            toLabels <- Task.gather(toLabelsJson.list.toList.flatMap(_.flatMap(_.string).map(context.expandIri).map(iri => graph.ns.classtypes
                              .get(context.expandIri(iri))
                              .flatMap(_.map(Task.now).getOrElse(
                                Task.raiseError(FromJsonException(s"$toLabelsJson classtypes could not be build/fetched")))))))
                          } yield {
                            (property, fromLabels, toLabels)
                            }).flatMap {
                          case (property, List(dt1: DataType[_]), List(dt2: DataType[_])) =>
                            Observable
                              .fromIterable(value.list.getOrElse(throw FromJsonException("array expected")))
                              .mapParallelUnordered(20)(_.list
                                .map {
                                  case List(from, id, to) =>
                                    for {
                                      fromResource <- decoder.toResource(from, Some(dt1))
                                      toResource <- decoder.toResource(to, Some(dt2))
                                    } yield {
                                      id.long.get -> (fromResource --- property --> toResource).id
                                    }
                                  case _ => throw FromJsonException(s"expected an array of size 2")
                                }
                                .getOrElse(throw FromJsonException("array expected")))
                          case (property, fromLabels, List(dt2: DataType[_])) =>
                            Observable
                              .fromIterable(value.list.getOrElse(throw FromJsonException("array expected")))
                              .mapParallelUnordered(20)(_.list
                                .map {
                                  case List(from, id, to) =>
                                    for {
                                      fromResource <- decoder
                                        .tryNodeRef(from)
                                        .getOrElse(throw FromJsonException("cannot parse noderef"))
                                      toResource <- decoder.toResource(to, Some(dt2))
                                    } yield {
                                      id.long.get -> (fromResource --- property --> toResource).id
                                    }
                                  case _ => throw FromJsonException(s"expected an array of size 2")
                                }
                                .getOrElse(throw FromJsonException("array expected")))
                          case (property, List(dt1: DataType[_]), toLabels) =>
                            Observable
                              .fromIterable(value.list.getOrElse(throw FromJsonException("array expected")))
                              .mapParallelUnordered(20)(_.list
                                .map {
                                  case List(from, id, to) =>
                                    for {
                                      fromResource <- decoder.toResource(from, Some(dt1))
                                      toResource <- decoder
                                        .tryNodeRef(to)
                                        .getOrElse(throw FromJsonException("cannot parse noderef"))
                                    } yield {
                                      id.long.get -> (fromResource --- property --> toResource).id
                                    }
                                  case _ => throw FromJsonException(s"expected an array of size 2")
                                }
                                .getOrElse(throw FromJsonException("array expected")))
                          case (property, fromLabels, toLabels) =>
                            Observable
                              .fromIterable(value.list.getOrElse(throw FromJsonException("array expected")))
                              .mapParallelUnordered(20)(_.list
                                .map {
                                  case List(from, id, to) =>
                                    for {
                                      fromResource <- decoder
                                        .tryNodeRef(from)
                                        .getOrElse(throw FromJsonException("cannot parse noderef"))
                                      toResource <- decoder
                                        .tryNodeRef(to)
                                        .getOrElse(throw FromJsonException("cannot parse noderef"))
                                    } yield {
                                      id.long.get -> (fromResource --- property --> toResource).id
                                    }
                                  case _ => throw FromJsonException(s"expected an array of size 2")
                                }
                                .getOrElse(throw FromJsonException("array expected")))
                          }
                  }
                  .getOrElse(Observable.raiseError(FromJsonException("line should be a json-object")))
              }
//              .append(List()) //TEMP-FIX: https://github.com/monix/monix/issues/832 reducing on an observable with only one item results in an empty stream
//              .reduce(_ ++ _)
              .toListL.map(_.toMap)
              .map(idmap => idMaps.copy(edgeIds = idmap))
//            .map(_.reduce(_ ++ _))
//              .completedL
//              .map { u =>
//                idMaps
//              }
          }
      }
  }

  private def readStructuredEdges(idMaps: IdMaps): Task[IdMaps] = {
    scribe.info(s"read structures edges ${graph.iri}")
    val decoder: DecodeLDFS[Json] = DecodeLDFS(graph, idMaps)
    graphfiles.read.context.structuredEdges
      .use(parseContext)
      .flatMap { implicit context =>
        graphfiles.read.structuredEdges
          .use { buf =>
            parse(buf)
              .flatMap { json =>
                json.list
                  .map {
                    case List(keyJson, fromLabelsJson, toLabelsJson, value) =>
                      Observable
                        .fromTask(
                          for {
                            property <- graph.ns.properties
                              .get(context.expandIri(keyJson.string.get))
                              .flatMap(_.map(Task.now).getOrElse(
                                Task.raiseError(FromJsonException(s"$keyJson property could not be build/fetched"))))
                            fromLabels <- Task.gather(fromLabelsJson.list.toList.flatMap(_.flatMap(_.string).map(context.expandIri).map(iri =>graph.ns.classtypes
                              .get(iri)
                              .flatMap(_.map(Task.now).getOrElse(
                                CollectionType.get(iri).map(Task.now).getOrElse(Task.raiseError(FromJsonException(s"$toLabelsJson classtypes could not be build/fetched")))
                              )))))
                            toLabels <- Task.gather(toLabelsJson.list.toList.flatMap(_.flatMap(_.string).map(context.expandIri).map(iri => graph.ns.classtypes
                              .get(iri)
                              .flatMap(_.map(Task.now).getOrElse(
                                CollectionType.get(iri).map(Task.now).getOrElse(Task.raiseError(FromJsonException(s"$toLabelsJson classtypes could not be build/fetched")))
                              )))))
                          } yield {
                            (property, fromLabels, toLabels)
                          }).flatMap {
                        case (property, List(dt1: DataType[_]), List(dt2: DataType[_])) =>
                          Observable
                            .fromIterable(value.list.getOrElse(throw FromJsonException("array expected")))
                            .mapParallelUnordered(20)(_.list
                              .map {
                                case List(from, id, to) =>
                                  for {
                                    fromResource <- decoder.toResource(from, Some(dt1))
                                    toResource <- decoder.toResource(to, Some(dt2))
                                  } yield {
                                    id.long.get -> (fromResource --- property --> toResource).id
                                  }
                                case _ => throw FromJsonException(s"expected an array of size 2")
                              }
                              .getOrElse(throw FromJsonException("array expected")))
                        case (property, fromLabels, List(dt2: DataType[_])) =>
                          Observable
                            .fromIterable(value.list.getOrElse(throw FromJsonException("array expected")))
                            .mapParallelUnordered(20)(_.list
                              .map {
                                case List(from, id, to) =>
                                  for {
                                    fromResource <- decoder
                                      .tryNodeRef(from).orElse(decoder.tryEdgeRef(from, fromLabels.head.asInstanceOf[Property]))
                                      .getOrElse(throw FromJsonException("cannot parse noderef"))
                                    toResource <- decoder.toResource(to, Some(dt2))
                                  } yield {
                                    id.long.get -> (fromResource --- property --> toResource).id
                                  }
                                case _ => throw FromJsonException(s"expected an array of size 2")
                              }
                              .getOrElse(throw FromJsonException("array expected")))
                        case (property, List(dt1: DataType[_]), toLabels) =>
                          Observable
                            .fromIterable(value.list.getOrElse(throw FromJsonException("array expected")))
                            .mapParallelUnordered(20)(_.list
                              .map {
                                case List(from, id, to) =>
                                  for {
                                    fromResource <- decoder.toResource(from, Some(dt1))
                                    toResource <- decoder
                                      .tryNodeRef(to).orElse(decoder.tryEdgeRef(to, toLabels.head.asInstanceOf[Property]))
                                      .getOrElse(throw FromJsonException("cannot parse noderef")).asInstanceOf[Task[Resource[Any]]]
                                  } yield {
                                    id.long.get -> (fromResource --- property --> toResource).id
                                  }
                                case _ => throw FromJsonException(s"expected an array of size 2")
                              }
                              .getOrElse(throw FromJsonException("array expected")))
                        case (property, fromLabels, toLabels) =>
                          Observable
                            .fromIterable(value.list.getOrElse(throw FromJsonException("array expected")))
                            .mapParallelUnordered(20)(_.list
                              .map {
                                case List(from, id, to) =>
                                  for {
                                    fromResource <- decoder
                                      .tryNodeRef(from)
                                      .getOrElse(throw FromJsonException("cannot parse noderef"))
                                    toResource <- decoder
                                      .tryNodeRef(to)
                                      .getOrElse(throw FromJsonException("cannot parse noderef"))
                                  } yield {
                                    id.long.get -> (fromResource --- property --> toResource).id
                                  }
                                case _ => throw FromJsonException(s"expected an array of size 2")
                              }
                              .getOrElse(throw FromJsonException("array expected")))
                      }
                  }
                  .getOrElse(Observable.raiseError(FromJsonException("line should be a json-object")))
              }
              .onErrorHandle { case e => scribe.error(e.getMessage); throw e }
//              .completedL
//              .map { u =>
//                idMaps
//              }
              .toListL.map(_.toMap)
              .map(idmap => idMaps.copy(edgeIds = idmap))
          }
      }
  }

  private def readStructures(idMaps: IdMaps): Task[IdMaps] = {
    scribe.info(s"read structures ${graph.iri}")
    val decoder: DecodeLDFS[Json] = DecodeLDFS(graph, idMaps)
    graphfiles.read.context.structures
      .use(parseContext)
      .flatMap { implicit context =>
        graphfiles.read.structures
          .use { buf =>
            parse(buf)
              .mapEval { json =>
                json.list
                  .map {
                    case List(id, label, value) =>
                      (for {
                        longId <- id.long
                        datatype <- label.string
                          .map(context.expandIri)
                          .flatMap { iri =>
                            CollectionType.get(iri)
                          }
                          .collect { case dt: DataType[_] => dt }
                      } yield {
                        decoder.toValue(value.asInstanceOf[decoder.Json], datatype).map(_.id).map(longId -> _)
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

  def writeLiterals(f: PrintWriter) = Task {
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
            f.println(Map(label.iri -> jsons.toList.asJson).asJson.noSpaces)
          }
      }
  }
  def writeNodes(f: PrintWriter) = Task {
    graph
      .nodes()
      .groupBy(_.labels.toSet)
      .foldLeft(ActiveContext()) {
        case (ac, (labels, nodes)) =>
          val (newAc, cLabels) = labels.foldLeft(ac -> List[String]()) {
            case ((ac, labels), label) =>
              val (cIri, newAc) = ac.compactIri(label)
              newAc -> (cIri :: labels)
          }
          val json = List(cLabels.map(_.asJson).asJson, nodes.map(_.id.asJson).toList.asJson).asJson
          f.println(json.noSpaces)
          newAc
      }
  }

  def writeLiteralEdges(f: PrintWriter) = Task {
    graph
      .edges()
      .filter(
        e =>
          (e.from.isInstanceOf[Node] || (!e.from.isInstanceOf[Edge[_,_]] && e.from.hasLabel(CollectionType.datatype).isEmpty)) && (e.to
            .isInstanceOf[Node] || (!e.to.isInstanceOf[Edge[_,_]] && e.to.hasLabel(CollectionType.datatype).isEmpty)))
      .groupBy(_.key)
      .foldLeft(ActiveContext()) {
        case (ac, (key, edges)) =>
          val (cKeyIri, _ac) = ac.compactIri(key)
          val groupedEdges   = edges.groupBy(e => e.from.labels -> e.to.labels)
          groupedEdges.foldLeft(_ac) {
            case (ac, ((List(dt1: DataType[_]), List(dt2: DataType[_])), edges)) =>
              val (newAc, jsons) = edges.foldLeft(ac -> List[Json]()) {
                case ((ac, jsons), edge) =>
                  val jip  = encoder.fromAny(edge.to, Some(dt1))(ac)
                  val jip2 = encoder.fromAny(edge.from, Some(dt2))(jip.activeContext)
                  jip2.activeContext -> (List(jip2.json, edge.id.asJson, jip.json).asJson :: jsons)
              }
              val fromTypes = List(dt1.iri.asJson).asJson
              val toTypes   = List(dt2.iri.asJson).asJson
              jsons.sliding(50, 50).foreach { jsons =>
                f.println(List(cKeyIri.asJson, fromTypes, toTypes, jsons.asJson).asJson.noSpaces)
              }
              newAc
            case (ac, ((fromLabels, List(dt2: DataType[_])), edges)) =>
              val (newAc, jsons) = edges.foldLeft(ac -> List[Json]()) {
                case ((ac, jsons), edge) =>
                  val jip = encoder.fromAny(edge.to, Some(dt2))(ac)
                  jip.activeContext -> (List(edge.from.id.asJson, edge.id.asJson, jip.json).asJson :: jsons)
              }
              val fromTypes = fromLabels.map(_.iri.asJson).asJson
              val toTypes   = List(dt2.iri.asJson).asJson
              jsons.sliding(50, 50).foreach { jsons =>
                f.println(List(cKeyIri.asJson, fromTypes, toTypes, jsons.asJson).asJson.noSpaces)
              }
              newAc
            case (ac, ((List(dt1: DataType[_]), toLabels), edges)) =>
              val (newAc, jsons) = edges.foldLeft(ac -> List[Json]()) {
                case ((ac, jsons), edge) =>
                  val jip = encoder.fromAny(edge.from, Some(dt1))(ac)
                  jip.activeContext -> (List(jip.json, edge.id.asJson, edge.to.id.asJson).asJson :: jsons)
              }
              val fromTypes = List(dt1.iri.asJson).asJson
              val toTypes   = toLabels.map(_.iri.asJson).asJson
              jsons.sliding(50, 50).foreach { jsons =>
                f.println(List(cKeyIri.asJson, fromTypes, toTypes, jsons.asJson).asJson.noSpaces)
              }
              newAc
            case (ac, ((fromLabels, toLabels), edges)) =>
              val (newAc, jsons) = edges.foldLeft(ac -> List[Json]()) {
                case ((ac, jsons), edge) =>
                  ac -> (List(edge.from.id.asJson, edge.id.asJson, edge.to.id.asJson).asJson :: jsons)
              }
              val fromTypes = fromLabels.map(_.iri.asJson).asJson
              val toTypes   = toLabels.map(_.iri.asJson).asJson
              jsons.sliding(50, 50).foreach { jsons =>
                f.println(List(cKeyIri.asJson, fromTypes, toTypes, jsons.asJson).asJson.noSpaces)
              }
              newAc
          }
      }
  }

  def writeStructuredEdges(f: PrintWriter) = Task {
    graph
      .edges()
      .filterNot(
        e =>
          (e.from.isInstanceOf[Node] || (!e.from.isInstanceOf[Edge[_,_]] && e.from.hasLabel(CollectionType.datatype).isEmpty)) && (e.to
            .isInstanceOf[Node] || (!e.to.isInstanceOf[Edge[_,_]] && e.to.hasLabel(CollectionType.datatype).isEmpty)))
      .groupBy(_.key)
      .foldLeft(ActiveContext()) {
        case (ac, (key, edges)) =>
          val (cKeyIri, _ac) = ac.compactIri(key)
          val groupedEdges   = edges.groupBy(e => e.from.labels -> e.to.labels)
          groupedEdges.foldLeft(_ac) {
            case (ac, ((List(dt1: DataType[_]), List(dt2: DataType[_])), edges)) =>
              val (newAc, jsons) = edges.foldLeft(ac -> List[Json]()) {
                case ((ac, jsons), edge) =>
                  val jip  = encoder.fromAny(edge.to, Some(dt1))(ac)
                  val jip2 = encoder.fromAny(edge.from, Some(dt2))(jip.activeContext)
                  jip2.activeContext -> (List(jip2.json, edge.id.asJson, jip.json).asJson :: jsons)
              }
              val fromTypes = List(dt1.iri.asJson).asJson
              val toTypes   = List(dt2.iri.asJson).asJson
              jsons.sliding(50, 50).foreach { jsons =>
                f.println(List(cKeyIri.asJson, fromTypes, toTypes, jsons.asJson).asJson.noSpaces)
              }
              newAc
            case (ac, ((fromLabels, List(dt2: DataType[_])), edges)) =>
              val (newAc, jsons) = edges.foldLeft(ac -> List[Json]()) {
                case ((ac, jsons), edge) =>
                  val jip = encoder.fromAny(edge.to, Some(dt2))(ac)
                  jip.activeContext -> (List(edge.from.id.asJson, edge.id.asJson, jip.json).asJson :: jsons)
              }
              val fromTypes = fromLabels.map(_.iri.asJson).asJson
              val toTypes   = List(dt2.iri.asJson).asJson
              jsons.sliding(50, 50).foreach { jsons =>
                f.println(List(cKeyIri.asJson, fromTypes, toTypes, jsons.asJson).asJson.noSpaces)
              }
              newAc
            case (ac, ((List(dt1: DataType[_]), toLabels), edges)) =>
              val (newAc, jsons) = edges.foldLeft(ac -> List[Json]()) {
                case ((ac, jsons), edge) =>
                  val jip = encoder.fromAny(edge.from, Some(dt1))(ac)
                  jip.activeContext -> (List(jip.json, edge.id.asJson, edge.to.id.asJson).asJson :: jsons)
              }
              val fromTypes = List(dt1.iri.asJson).asJson
              val toTypes   = toLabels.map(_.iri.asJson).asJson
              jsons.sliding(50, 50).foreach { jsons =>
                f.println(List(cKeyIri.asJson, fromTypes, toTypes, jsons.asJson).asJson.noSpaces)
              }
              newAc
            case (ac, ((fromLabels, toLabels), edges)) =>
              val (newAc, jsons) = edges.foldLeft(ac -> List[Json]()) {
                case ((ac, jsons), edge) =>
                  ac -> (List(edge.from.id.asJson, edge.id.asJson, edge.to.id.asJson).asJson :: jsons)
              }
              val fromTypes = fromLabels.map(_.iri.asJson).asJson
              val toTypes   = toLabels.map(_.iri.asJson).asJson
              jsons.sliding(50, 50).foreach { jsons =>
                f.println(List(cKeyIri.asJson, fromTypes, toTypes, jsons.asJson).asJson.noSpaces)
              }
              newAc
          }
      }
  }

  def writeStructures(f: PrintWriter) = Task {
    graph
      .values()
      .flatMap(_.hasLabel(StructuredType.datatype))
      .foldLeft(ActiveContext()) {
        case (ac, v) =>
          val jip            = encoder.fromStructured(v.value, v.label.asInstanceOf[StructuredType[_]])(ac)
          val (label, newAc) = jip.activeContext.compactIri(v.label)
          f.println(List(v.id.asJson, label.asJson, jip.json).asJson.noSpaces)
          newAc
      }
  }

  override def persist: Task[Unit] = {
    val encoder = EncodeLDFS()
    scribe.info(s"persisting ${graph.iri} to $path")

    Task
      .gatherUnordered(Seq(
        graphfiles.write.literals.use(writeLiterals).onErrorHandle { f =>
          println(f.getMessage); throw f
        },
        graphfiles.write.nodes
          .use(writeNodes)
          .onErrorHandle { f =>
            println(f.getMessage); throw f
          }
          .flatMap { ac =>
            graphfiles.write.context.nodes.use { f =>
              Task {
                f.println(ac.asJson
                  .map(_.noSpaces)
                  .getOrElse(""))
              }
            }
          },
        graphfiles.write.literalEdges
          .use(writeLiteralEdges)
          .onErrorHandle { f =>
            println(f.getMessage); throw f
          }
          .flatMap { ac =>
            graphfiles.write.context.literalEdges.use { f =>
              Task {
                f.println(ac.asJson.map(_.noSpaces).getOrElse(""))
              }
            }
          },
        graphfiles.write.structuredEdges
          .use(writeStructuredEdges)
          .onErrorHandle { f =>
            println(f.getMessage); throw f
          }
          .flatMap { ac =>
            graphfiles.write.context.structuredEdges.use { f =>
              Task {
                f.println(ac.asJson.map(_.noSpaces).getOrElse(""))
              }
            }
          },
        graphfiles.write.structures
          .use(writeStructures)
          .onErrorHandle { f =>
            println(f.getMessage); throw f
          }
          .flatMap { ac =>
            graphfiles.write.context.structures.use { f =>
              Task {
                f.println(ac.asJson.map(_.noSpaces).getOrElse(""))
              }
            }
          }
      ))
      .onErrorHandle { f =>
        println(f.getMessage); throw f
      }
      .foreachL(f => {})
  }

  /**
    * finishes write-queue(s) and closes connection
    */
  override def close(): Task[Unit] = Task.unit//CancelableFuture.unit //persist
}
