package lspace.codec

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.concurrent.ConcurrentHashMap

import lspace.Label
import lspace.NS.types
import lspace.codec.exception.{FromJsonException, NotAcceptableException, UnexpectedJsonException}
import lspace.datatype._
import lspace.structure._
import lspace.parse.util.{HttpClient, HttpClientImpl}
import lspace.types.vector.{Geometry, Point, Polygon}
import monix.eval.{Coeval, Task}
import monix.reactive.Observable

import scala.annotation.tailrec
import scala.collection.{concurrent, immutable}
import scala.collection.immutable.{AbstractMap, Iterable, ListSet, Map, MapLike}
import scala.collection.JavaConverters._

object ExpandedMap {
  def apply[V](obj: Map[String, V])(implicit activeContext: ActiveContext): ExpandedMap[V] =
    new ExpandedMap(activeContext.expandKeys(obj))
}
class ExpandedMap[V](val obj: Map[String, V]) {
  def get(key: String)      = obj.get(key)
  def contains(key: String) = obj.contains(key)
  def size                  = obj.size
  def isEmpty               = obj.isEmpty
  def nonEmpty              = obj.nonEmpty
  def keys                  = obj.keys

  def -(key: String): ExpandedMap[V] = new ExpandedMap[V](obj - key)
}
object Decoder {
  type Aux[Json0] = Decoder { type Json = Json0 }

  def apply[Json0](graph0: Lspace)(implicit
                                   baseDecoder0: NativeTypeDecoder.Aux[Json0]): Decoder.Aux[Json0] =
    new Decoder {
      type Json = Json0
      val graph: Graph                                      = graph0
      implicit def baseDecoder: NativeTypeDecoder.Aux[Json] = baseDecoder0
      lazy val nsDecoder = new Decoder {
        type Json = Json0
        val graph: Graph                                      = graph0.ns
        implicit def baseDecoder: NativeTypeDecoder.Aux[Json] = baseDecoder0
        lazy val nsDecoder                                    = this
      }
    }
}

trait Decoder {
  def graph: Graph
  def nsDecoder: Decoder
  type Json
  implicit def baseDecoder: NativeTypeDecoder.Aux[Json]

  def apply(graph0: Lspace): Decoder.Aux[Json] = Decoder.apply(graph0)(baseDecoder)

//  implicit def decoder: Decoder = this
  type AC = ActiveContext
  type AP = ActiveProperty
  def parse(string: String): Task[Json] = baseDecoder.parse(string)

  implicit def jsonToList(json: Json): Option[List[Json]]             = baseDecoder.jsonToList(json)
  implicit def jsonToMap(json: Json): Option[Map[String, Json]]       = baseDecoder.jsonToMap(json)
  implicit def jsonToString(json: Json): Option[String]               = baseDecoder.jsonToString(json)
  implicit def jsonToBoolean(json: Json): Option[Boolean]             = baseDecoder.jsonToBoolean(json)
  implicit def jsonToInt(json: Json): Option[Int]                     = baseDecoder.jsonToInt(json)
  implicit def jsonToDouble(json: Json): Option[Double]               = baseDecoder.jsonToDouble(json)
  implicit def jsonToLong(json: Json): Option[Long]                   = baseDecoder.jsonToLong(json)
  implicit def jsonToDateTime(json: Json): Option[Instant]            = json.string.map(Instant.parse(_))
  implicit def jsonToLocalDateTime(json: Json): Option[LocalDateTime] = json.string.map(LocalDateTime.parse(_))
  implicit def jsonToDate(json: Json): Option[LocalDate]              = json.string.map(LocalDate.parse(_))
  implicit def jsonToTime(json: Json): Option[LocalTime]              = json.string.map(LocalTime.parse(_))
  implicit def jsonToGeopoint(json: Json): Option[Point] =
    lspace.decode.fromGeoJson(json).toOption.collect { case point: Point => point }
  implicit def jsonToGeopolygon(json: Json): Option[Polygon] =
    lspace.decode.fromGeoJson(json).toOption.collect { case polygon: Polygon => polygon }

  implicit class WithDJson(json: Json) {
    def isNull: Boolean                      = baseDecoder.jsonIsNull(json)
    def int: Option[Int]                     = jsonToInt(json)
    def double: Option[Double]               = jsonToDouble(json)
    def long: Option[Long]                   = jsonToLong(json)
    def localdatetime: Option[LocalDateTime] = jsonToLocalDateTime(json)
    def datetime: Option[Instant]            = jsonToDateTime(json)
    def time: Option[LocalTime]              = jsonToTime(json)
    def date: Option[LocalDate]              = jsonToDate(json)
    def string: Option[String]               = jsonToString(json)
    def list: Option[List[Json]]             = jsonToList(json)
    def obj: Option[Map[String, Json]]       = jsonToMap(json)
    def boolean: Option[Boolean]             = jsonToBoolean(json)
//    def geo: Option[Geometry] = ???
    def geoPoint: Option[Point]     = jsonToGeopoint(json)
    def geoPolygon: Option[Polygon] = jsonToGeopolygon(json)
  }

  def stringToLabeledNode(json: String, ontology: Ontology, activeContext: AC = ActiveContext()): Task[Node] =
    parse(json)
      .flatMap(toLabeledNode(_, ontology, activeContext))

  def toLabeledNode(json: Json, ontology: Ontology, activeContext: AC = ActiveContext()): Task[Node] = {
    json.obj
      .map { obj =>
        extractContext(obj)(activeContext).flatMap { implicit activeContext =>
          val expandedJson = ExpandedMap(obj)
          extractOntologies(expandedJson).flatMap { ontologies =>
            if (ontologies.contains(ontology)) toNode(expandedJson, None)(activeContext)
            else
              Task.raiseError(NotAcceptableException(s"cannot parse root object, expected @type ${ontology.iri}"))
          }
        }
      }
      .getOrElse(Task.raiseError(FromJsonException("root must be an object")))
  }

  def stringToNode(json: String, activeContext: AC = ActiveContext()): Task[Node] =
    parse(json)
      .flatMap(toNode(_, activeContext))

  def toNode(json: Json, activeContext: AC = ActiveContext()): Task[Node] = {
    json.obj
      .map { obj =>
        extractContext(obj)(activeContext).flatMap { implicit activeContext =>
          val expandedJson = ExpandedMap(obj)
          if (expandedJson.contains(types.`@value`))
            Task.raiseError(FromJsonException("cannot parse object with @value key to node, this looks like a value"))
          else if (expandedJson.contains(types.`@from`))
            Task.raiseError(FromJsonException("cannot parse object with @from key to node, this looks like an edge"))
          else if (expandedJson.contains(types.`@to`))
            Task.raiseError(FromJsonException("cannot parse object with @to key to node, this looks like an edge"))
          else {
            toNode(expandedJson, None)(activeContext)
          }
        }
      }
      .getOrElse(Task.raiseError(FromJsonException("root must be an object")))
  }

  /**
    * @param resource
    * @param property
    * @param obj
    * @param activeContext
    * @tparam T
    * @return
    */
  def toResource(expandedJson: ExpandedMap[Json], expectedType: Option[ClassType[_]])(
      implicit activeContext: AC): Task[Resource[Any]] = {
//    extractContext(obj).flatMap { implicit activeContext =>
//      val expandedJson = activeContext.expandKeys(obj)
    extractType(expandedJson).flatMap { expectedTypes =>
      val et = if (expectedTypes.nonEmpty) expectedTypes else expectedType.toList
      tryValue(expandedJson, et.collectFirst { case datatype: DataType[_] => datatype })
        .orElse(toEdge(expandedJson, et.collectFirst { case property: Property => property }))
        .getOrElse {
          et match {
//              case (geo: GeometricType[_]) :: tail =>
//                toGeometric(encoder.encode(obj), geo).map { v =>
//                  graph.values.create(v, geo)
//                }
            case _ =>
              toNode(expandedJson, expectedType.collect { case ontology: Ontology => ontology })
          }
        }
    }
  }

  def toResource(json: Json, expectedType: Option[ClassType[_]])(implicit activeContext: AC): Task[Resource[Any]] = {
    json.obj
      .map { obj =>
        extractContext(obj).flatMap { implicit activeContext =>
          val expandedJson = ExpandedMap(obj)
          if (expectedType.exists(_.isInstanceOf[GeometricType[_]]))
            toGeometric(json, expectedType.get.asInstanceOf[GeometricType[_]])
              .map(geo => graph.values.upsert(geo, expectedType.get.asInstanceOf[DataType[Any]]))
              .onErrorHandleWith { f =>
                toResource(expandedJson, expectedType)
              } else toResource(expandedJson, expectedType)
        }
      }
      .getOrElse(toObject(json, expectedType.toList).map {
        case (classtype, resource: Resource[_]) => resource
        case (classtype: DataType[_], value)    => graph.values.create(value, classtype)
      })
  }

  /**
    *
    * @param resource
    * @param otherJson already expanded object
    * @param activeContext
    * @tparam T
    * @return
    */
  def withEdges[T <: Resource[_]](resource: T, otherJson: ExpandedMap[Json])(implicit activeContext: AC): Task[T] = {
    Task
      .gatherUnordered(otherJson.obj.map {
        case (key, value) =>
          activeContext.definitions
            .get(key)
            .map(_ -> value)
            .map(Task.now)
            .getOrElse(toProperty(key).map(property => ActiveProperty(property = property) -> value))
      })
      .map(_.map {
        case (activeProperty, json) =>
          val property = activeProperty.property
          val addEdgeF =
            if (activeProperty.`@reverse`) (value: Resource[_]) => resource.addIn(property, value)
            else (value: Resource[_]) => resource.addOut(property, value)
          val addEdgeTypedF =
            if (activeProperty.`@reverse`)(ct: ClassType[Any], value: Any) => resource.addIn(property, ct, value)
            else (ct: ClassType[Any], value: Any) => resource.addOut(property, ct, value)
          json.obj
            .map(ExpandedMap(_))
            .map(toResource(_, Some(property)).map(addEdgeF(_)).map(List(_)))
            .orElse(json.list.map {
              array =>
                val edgesTask: Task[List[Edge[Any, Any]]] =
                  activeProperty.`@container` match {
                    case Nil =>
                      val expectedType = activeContext.expectedType(property)
                      expectedType
                        .map {
                          case collectionType: CollectionType[_] =>
                            toCollection(array, collectionType)
                              .map(addEdgeTypedF(collectionType, _))
                              .map(List(_))
                          case et =>
                            //                            if (property.iri == types.schemaDomainIncludes) {
                            toCollection(array, ListType(List(et)))
                              .map(nodes => nodes.map(node => addEdgeTypedF(et, node)))
                          //                            } else
                          //                              Task.raiseError(FromJsonException(
                          //                                s"array found for ${property.iri} with expected type ${et.iri} in ${resource.iri} without @collection type or @container:@list/@set ${array}"))
                        }
                        .getOrElse {
                          //TODO: duplicated code, this processes unexpected arrays
                          Task.gatherUnordered(array.map {
                            json =>
                              json.obj
                                .map(ExpandedMap(_))
                                .map(toResource(_, Some(property)).map(addEdgeF(_)))
                                .orElse(
                                  json.list
                                    .map { array =>
                                      val expectedType = activeContext.expectedType(property)
                                      expectedType match {
                                        case Some(collectionType: CollectionType[_]) =>
                                          toCollection(array, collectionType).map(addEdgeTypedF(collectionType, _))
                                        case _ =>
                                          Task.raiseError(FromJsonException("array found without @collection type"))
                                      }
                                    }
                                )
                                .orElse {
                                  for {
                                    iri <- json.string
                                    expectedType <- activeContext.expectedType(property).collect {
                                      case ontology: Ontology => ontology
                                    }
                                  } yield {
                                    Task.now(graph.nodes.upsert(iri, expectedType)).map(addEdgeF(_))
                                  }
                                }
                                .orElse(toPrimitive(json)
                                  .map(v => addEdgeTypedF(ClassType.valueToOntologyResource(v), v))
                                  .map(Task.now))
                                .getOrElse(Task.raiseError(FromJsonException("cannot parse value")))
                          })
                          //                          Task.raiseError(FromJsonException(
                          //                            s"array found for ${property.iri} without expected @collection type or @container:@list/@set"))
                        }
                    case containers
                        if containers.headOption.exists(
                          h => Set[`@container`](`@container`.`@list`, `@container`.`@set`).contains(h)) =>
                      Task.gatherUnordered(array.map {
                        json =>
                          json.obj
                            .map(ExpandedMap(_))
                            .map(toResource(_, Some(property)).map(addEdgeF(_)))
                            .orElse(
                              json.list
                                .map { array =>
                                  val expectedType = activeContext.expectedType(property)
                                  expectedType match {
                                    case Some(collectionType: CollectionType[_]) =>
                                      toCollection(array, collectionType).map(addEdgeTypedF(collectionType, _))
                                    case _ =>
                                      Task.raiseError(FromJsonException("array found without @collection type"))
                                  }
                                }
                            )
                            .orElse {
                              for {
                                iri <- json.string
                                expectedType <- activeContext.expectedType(property).collect {
                                  case ontology: Ontology => ontology
                                }
                              } yield {
                                Task.now(graph.nodes.upsert(iri, expectedType)).map(addEdgeF(_))
                              }
                            }
                            .orElse(toPrimitive(json)
                              .map(v => addEdgeTypedF(ClassType.valueToOntologyResource(v), v))
                              .map(Task.now))
                            .getOrElse(Task.raiseError(FromJsonException("cannot parse value")))
                      })
                  }
                edgesTask
            })
            .getOrElse {
              val expectedType = activeContext.expectedType(property)
              (for {
                iri <- json: Option[String]
                et  <- expectedType.collect { case ontology: Ontology => ontology }
              } yield {
                Task.now(graph.nodes.upsert(iri, et)).map(v => addEdgeF(v))
              }).orElse {
                  expectedType.collect { case datatype: DataType[_] => datatype }.map { label =>
                    toValue(json, label).map(v => addEdgeF(v))
                  }
                }
                .orElse(toPrimitive(json)
                  .map(v => graph.values.create(v, ClassType.valueToOntologyResource(v)))
                  .map(v => addEdgeF(v))
                  .map(Task.now(_)))
                .getOrElse(Task.raiseError(FromJsonException("cannot parse @value")))
                .map(List(_))
            }
        //              .getOrElse {
        //                toObject[Any](json).map {
        //                  case (label, value) =>
        //                    resource.addOut(property, label, value)
        //                }
        //              }
      })
      .flatMap { l =>
        Task.gatherUnordered(l).map(t => resource)
      }
  }

  def tryNodeRef(json: Json)(implicit activeContext: AC): Option[Task[Node]] =
    json.string
      .map(activeContext.expandIri)
      .map(graph.nodes.upsert(_)) //TODO: add label if missing?
      .map(Task.now)
  //        .getOrElse(Task.raiseError(FromJsonException("object expected when parsing to node")))

  def toNode(expandedJson: ExpandedMap[Json], label: Option[Ontology])(implicit activeContext: AC): Task[Node] = {
    val iri = activeContext.extractId(expandedJson)
    if (iri.isDefined && expandedJson.size == 1) {
      //node-ref
      Task.now(graph.nodes.upsert(iri.get))
    } else {
      val iris = activeContext.extractIds(expandedJson).toSet
      val node = iri.map(graph.nodes.upsert(_, iris)).getOrElse(graph.nodes.create())
      extractOntologies(expandedJson).flatMap { ontologies =>
        if (ontologies.isEmpty) label.foreach(node.addLabel)
        else ontologies.foreach(node.addLabel)
        withEdges(node, expandedJson - types.`@context` - types.`@id` - types.`@ids` - types.`@type`)
      }
    }
  }

  def tryEdgeRef(json: Json, label: Property)(implicit activeContext: AC): Option[Task[Edge[_, _]]] =
    json.string
      .map(activeContext.expandIri)
      .flatMap(graph.edges.hasIri(_).headOption) //TODO: check if label == edge.key and throw exception if !=
      .map(Task.now)
  def toEdge(expandedJson: ExpandedMap[Json], expectedType: Option[Property])(
      implicit activeContext: AC): Option[Task[Edge[Any, Any]]] = toEdge(expandedJson, expectedType.toList)
  def toEdge(expandedJson: ExpandedMap[Json], expectedTypes: List[Property])(
      implicit activeContext: AC): Option[Task[Edge[Any, Any]]] = {
    (activeContext.extractFrom(expandedJson), activeContext.extractTo(expandedJson)) match {
      case (Some(source), Some(destination)) =>
        Some((for {
          from <- toResource(source, None)
          to   <- toResource(destination, None)
        } yield {
          extractProperty(expandedJson)
            .map(_.orElse(expectedTypes.headOption))
            .flatMap {
              _.filter(expectedTypes.contains)
                .map { label =>
                  val edge: Edge[Any, Any] = from.addOut(label, to)
                  withEdges(
                    edge,
                    expandedJson - types.`@context` - types.`@id` - types.`@ids` - types.`@from` - types.`@to` - types.`@type`)
                    .map(e => edge)
                }
                .getOrElse(Task.raiseError(FromJsonException("unexpected @type for edge object")))
            }
        }).flatten)
      case (Some(from), None) =>
        Some(Task.raiseError(FromJsonException("incomplete edge, missing @from")))
      case (None, Some(to)) =>
        Some(Task.raiseError(FromJsonException("incomplete edge, missing @from")))
      case _ => None
    }
  }

  def toLiteral[T](json: Json, label: LiteralType[T])(implicit activeContext: AC): Task[T] =
    (label match {
      case TextType.datatype =>
        json.string
      case tpe: NumericType[_] =>
        tpe match {
          case DoubleType.datatype => json.double
          case LongType.datatype   => json.long
          case IntType.datatype    => json.int
        }
      case tpe: CalendarType[_] =>
        tpe match {
          case LocalDateTimeType.datatype => json.localdatetime
          case DateTimeType.datatype      => json.datetime
          case LocalDateType.datatype     => json.date
          case LocalTimeType.datatype     => json.time
        }
      //        case tpe: ColorType[_] =>
      case BoolType.datatype => json.boolean
      case _                 => None
    }).map(_.asInstanceOf[T])
      .map(Task.now)
      .getOrElse(Task.raiseError(UnexpectedJsonException(s"unknown LiteralType ${label.iri}")))

  def toStructured[T](json: Json, label: StructuredType[T])(implicit activeContext: AC): Task[T] =
    label match {
      case label: GeometricType[T] =>
        toGeometric(json, label)
      case label: CollectionType[T] =>
        json.list.map(toCollection(_, label)).getOrElse(Task.raiseError(UnexpectedJsonException(s"not a @list")))
      case label: TupleType[T] =>
        json.list.map(toTuple(_, label)).getOrElse(Task.raiseError(UnexpectedJsonException(s"not a @tuple")))
      case _ => Task.raiseError(UnexpectedJsonException(s"unknown StructuredType ${label.iri}"))
    }

  def toGeometric[T](json: Json, label: GeometricType[T])(implicit activeContext: AC): Task[T] = {
    import lspace.decode._
    (label match { //TODO: create specific parsers
      case label: GeopointType[_]   => json.geoPoint
      case label: GeoPolygonType[_] => json.geoPolygon
      case _                        => None
    }).map(_.asInstanceOf[T])
      .map(Task.now)
      .getOrElse(Task.raiseError(UnexpectedJsonException(s"unknown GeometricType ${label.iri}")))
  }

  def toCollection[T](json: List[Json], label: CollectionType[T])(implicit activeContext: AC): Task[T] =
    label match {
      case label: ListType[_]    => toList(json, label.valueRange).map(_.asInstanceOf[T])
      case label: VectorType[_]  => toVector(json, label.valueRange).map(_.asInstanceOf[T])
      case label: SetType[_]     => toSet(json, label.valueRange).map(_.asInstanceOf[T])
      case label: ListSetType[_] => toListSet(json, label.valueRange).map(_.asInstanceOf[T])
      case label: MapType[_, _]  => toMap(json, label.keyRange, label.valueRange).map(_.asInstanceOf[T])
      case _                     => Task.raiseError(UnexpectedJsonException(s"unknown CollectionType ${label.iri}"))
    }

  def toTuple[T](json: List[Json], label: TupleType[T])(implicit activeContext: AC): Task[T] =
    label match {
      case dt: TupleType[_] =>
        if (json.size != dt.rangeTypes.size)
          Task.raiseError(UnexpectedJsonException("tuple range is not equal to tuple size"))
        else
          Task
            .gather(json.zip(dt.rangeTypes).map { case (json, types) => toObject(json, types) })
            .map(_.map(_._2))
            .map {
              case List(a, b)             => (a, b)
              case List(a, b, c)          => (a, b, c)
              case List(a, b, c, d)       => (a, b, c, d)
              case List(a, b, c, d, e)    => (a, b, c, d, e)
              case List(a, b, c, d, e, f) => (a, b, c, d, e, f)
            }
            .map(_.asInstanceOf[T])
      case _ => Task.raiseError(UnexpectedJsonException(s"unknown TupleType ${label.iri}"))
    }

  def toObject(json: Json, label: List[ClassType[_]])(implicit activeContext: AC): Task[(ClassType[Any], Any)] = {
    json.obj
      .map { obj =>
        extractContext(obj).flatMap { implicit activeContext =>
          val expandedJson = ExpandedMap(obj)
          activeContext
            .extractValue(expandedJson) { json =>
              extractDatatype(expandedJson).map(_.orElse(label.headOption)).flatMap { labelOption =>
                labelOption
                  .map {
                    case tpe: DataType[_] if label.nonEmpty && !label.contains(tpe) =>
                      Task.raiseError(UnexpectedJsonException("a collection can only have value with the @valueRange"))
                    case label: DataType[_] =>
                      toData(json, label).map(label -> _)
                    case _ =>
                      Task.raiseError(UnexpectedJsonException("@value can only have a label extending @datatype"))
                  }
                  .orElse(tryRaw(json))
                  .getOrElse(Task.raiseError(UnexpectedJsonException("cannot decode to value")))
              }
            }
            .orElse(toEdge(expandedJson, label.collect { case property: Property => property }).map(_.map(e =>
              e.key -> e)))
            .getOrElse(toNode(expandedJson, label.collectFirst { case ontology: Ontology => ontology }).map(n =>
              NodeURLType.datatype -> n))
        }
      }
      .orElse(label.headOption.flatMap {
        case label: Ontology => tryNodeRef(json).map(_.map(label -> _))
        case label: Property =>
          tryEdgeRef(json, label).map(_.map(label -> _))
        //            Some(Task.raiseError(
        //              UnexpectedJsonException(s"expected edge with @type ${label.iri} but json is not an object")))
        case label: DataType[_] => Some(toData(json, label).map(label -> _))
      })
      .orElse(tryRaw(json))
      .getOrElse(Task.raiseError(UnexpectedJsonException("cannot decode to value")))
  } //.asInstanceOf[Task[(ClassType[Any], Any)]]

  def tryRaw(json: Json)(implicit activeContext: AC): Option[Task[(ClassType[Any], Any)]] = {
    //is url
    //is primitive
    toPrimitive(json).map(v => ClassType.valueToOntologyResource(v) -> v).map(Task.now)
  }

  def toObject(expandedJson: ExpandedMap[Json], expectedType: Option[ClassType[_]])(
      implicit activeContext: AC): Task[(ClassType[Any], Any)] =
    activeContext
      .extractValue(expandedJson) { json =>
        extractDatatype(expandedJson)
          .map(_.orElse(expectedType))
          .flatMap {
            _.collect { case datatype: DataType[_] => datatype }
              .map { label =>
                toData(json, label).map(label -> _)
              }
              .getOrElse(Task.raiseError(UnexpectedJsonException("cannot parse value without type")))
          }
          .asInstanceOf[Task[(ClassType[Any], Any)]]
      }
      .orElse(toEdge(expandedJson, expectedType.collect { case property: Property => property }).map(_.map(e =>
        e.key -> e).asInstanceOf[Task[(ClassType[Any], Any)]]))
      .getOrElse {
        toNode(expandedJson, expectedType.collect {
          case ontology: Ontology => ontology
        }).map(n => NodeURLType.datatype.asInstanceOf[NodeURLType[Node]] -> n)
          .asInstanceOf[Task[(ClassType[Any], Any)]]
      }

  def tryData(expandedJson: ExpandedMap[Json], expectedType: Option[DataType[_]])(
      implicit activeContext: AC): Option[Task[Any]] = {
    activeContext
      .extractValue(expandedJson) { json =>
        extractDatatype(expandedJson).map(_.orElse(expectedType)).flatMap {
          _.map { label =>
            toData(json, label)
          }.getOrElse(Task.raiseError(UnexpectedJsonException("cannot parse value without type")))
        }
      }
  }

  def toData(json: Json, label: DataType[_])(implicit activeContext: AC): Task[Any] =
    label match {
      case label: LiteralType[Any] =>
        toLiteral(json, label)
      case label: StructuredType[Any] =>
        toStructured(json, label)
      case label: IriType[_] =>
        json.string
          .map(activeContext.expandIri)
          .map(graph.nodes.upsert(_))
          .map(Task(_))
          .getOrElse(Task.raiseError(UnexpectedJsonException(s"unknown IriType, expected IRI/URL ${json}")))
      case _ =>
        Task.raiseError(UnexpectedJsonException(s"unknown DataType ${label.iri}"))
    }

  def tryValue(expandedJson: ExpandedMap[Json], expectedType: Option[DataType[_]])(
      implicit activeContext: AC): Option[Task[Value[Any]]] = {
    activeContext
      .extractValue(expandedJson) { json =>
        extractDatatype(expandedJson)
          .map(_.orElse(expectedType))
          .flatMap { label =>
            label
              .map(toValue(json, _))
              .orElse(
                toPrimitive(json)
                  .map(v => graph.values.create(v, ClassType.valueToOntologyResource(v)))
                  .map(Task.now))
              .getOrElse(Task.raiseError(FromJsonException("cannot parse @value")))
          }
      }
  }

  def toValue(json: Json, label: DataType[_])(implicit activeContext: AC): Task[Value[Any]] =
    toData(json, label).map { v =>
      graph.values.create(v, label)
    }

  //Int, Long, Double or String
  def toPrimitive(json: Json): Option[Any] =
    json.long
      .orElse(json.double)
      .orElse(json.int)
      .orElse(json.string)

  /**
    * gets list or iris
    * @param json
    * @param activeContext
    * @return
    */
  def extractIris(json: Json)(implicit activeContext: AC): List[String] =
    json.list
      .map(array =>
        array.flatMap(json =>
          json.obj
            .flatMap { obj =>
              val expandedJson = ExpandedMap(obj)
              activeContext.extractId(expandedJson)
            }
            .orElse(json.string)))
      .orElse(json.string.map(List(_)))
      .orElse(json.obj
        .flatMap { obj =>
          val expandedJson = ExpandedMap(obj)
          activeContext.extractId(expandedJson)
        }
        .map(List(_)))
      .getOrElse(List())
      .map(activeContext.expandIri)

  def prepareOntologyNode(expandedJson: ExpandedMap[Json])(implicit activeContext: AC): Task[Node] = {
//    scribe.trace(s"prepare ontology ${obj}")
//    val expandedJson = activeContext.expandKeys(obj)
    if (expandedJson
          .get(types.`@type`)
          .map(extractIris(_))
          .exists(iris => Ontology.ontology.iris.intersect(iris.toSet).nonEmpty)) {
      toNode(expandedJson - types.`@extends` - types.rdfsSubClassOf - types.`@properties`, Some(Ontology.ontology))
        .onErrorHandle { f =>
          scribe.error(f.getMessage); throw f
        }
        .map { node =>
          val extendsIris = expandedJson
            .get(types.`@extends`)
            .orElse(expandedJson.get(types.rdfsSubClassOf))
            .map(extractIris(_))
          extendsIris
            .map(_.map(graph.nodes.upsert(_)))
            .map(node.addOut(Label.P.`@extends`, _))
          val propertiesIris = expandedJson
            .get(types.`@properties`)
            .map(extractIris(_))
          propertiesIris.map(_.map(graph.nodes.upsert(_))).map(_.map(node.addOut(Label.P.`@properties`, _)))
          node
        }
    } else Task.raiseError(FromJsonException(s"ontology is not of type '@class' ${expandedJson.obj}"))
  }

  def toOntology(iri: String)(implicit activeContext: AC): Task[Ontology] =
    graph.ns.ontologies
      .get(iri)
      .flatMap(_.map(Task.now)
        .getOrElse {
          nsDecoder.fetchOntologyNode(iri).toListL.flatMap {
            nodes =>
              val unfinished = nodes.filter(node =>
                node.hasLabel(Ontology.ontology).isEmpty && node.hasLabel(Property.ontology).isEmpty)
              Task.gather(unfinished.map(_.iri).map(toClasstype)).flatMap { clstypes =>
                graph.ns.ontologies
                  .get(iri)
                  .flatMap(_.map(Task.now).getOrElse(Task.raiseError(
                    FromJsonException(s"could not build ontology after having done all the preparing ... "))))
              }
          }
        })

  def toOntologies(json: Json)(implicit activeContext: AC): Task[List[Ontology]] =
    Task.gather(extractIris(json).map(toOntology))

  def preparePropertyNode(expandedJson: ExpandedMap[Json])(implicit activeContext: AC): Task[Node] = {
//    scribe.trace(s"prepare property ${obj}")
//    val expandedJson = activeContext.expandKeys(obj)
    if (expandedJson
          .get(types.`@type`)
          .map(extractIris(_))
          .exists(iris => Property.ontology.iris.intersect(iris.toSet).nonEmpty)) {
      toNode(expandedJson - types.`@range` - types.schemaRange - types.schemaDomainIncludes - types.`@properties`,
             Some(Property.ontology))
        .map { node =>
          val rangeIris = expandedJson
            .get(types.`@range`)
            .orElse(expandedJson.get(types.schemaRange))
            .map(extractIris(_))
          rangeIris
            .map(_.map(graph.nodes.upsert(_)))
            .map(node.addOut(Label.P.`@range`, _))
          val domainIncludeIris = expandedJson
            .get(types.schemaDomainIncludes)
            .map(extractIris(_))
          domainIncludeIris
            .map(_.map(graph.nodes.upsert(_)))
            .map(_.map(_.addOut(Label.P.`@properties`, node)))
          val extendsIris = expandedJson
            .get(types.`@extends`)
            .orElse(expandedJson.get(types.rdfsSubPropertyOf))
            .map(extractIris(_))
          extendsIris
            .map(_.map(graph.nodes.upsert(_)))
            .map(node.addOut(Label.P.`@extends`, _))
          val propertiesIris = expandedJson
            .get(types.`@properties`)
            .map(extractIris(_))
          propertiesIris.map(_.map(graph.nodes.upsert(_))).map(_.map(node.addOut(Label.P.`@properties`, _)))
          node
        }
    } else Task.raiseError(FromJsonException("property is not of type '@property'"))
  }

  def toProperty(iri: String)(implicit activeContext: AC): Task[Property] =
    graph.ns.properties
      .get(iri)
      .flatMap(_.map(Task.now)
        .getOrElse {
          nsDecoder.fetchPropertyNode(iri).toListL.flatMap {
            nodes =>
              val unfinished = nodes.filter(node =>
                node.hasLabel(Ontology.ontology).isEmpty && node.hasLabel(Property.ontology).isEmpty)
              Task.gather(unfinished.map(_.iri).map(toClasstype)).flatMap { clstypes =>
                graph.ns.properties
                  .get(iri)
                  .flatMap(_.map(Task.now).getOrElse(Task.raiseError(
                    FromJsonException("could not build property after having done all the preparing ..."))))
              }
          }
        })

  def toProperties(json: Json)(implicit activeContext: AC): Task[List[Property]] =
    Task.gather(extractIris(json).map(toProperty))

  private val building: concurrent.Map[String, Task[Node]] =
    new ConcurrentHashMap[String, Task[Node]]().asScala

  def prepareDataTypeNode(expandedJson: ExpandedMap[Json])(implicit activeContext: AC): Task[Node] = {
//    scribe.trace(s"prepare datatype ${obj}")
//    val expandedJson = activeContext.expandKeys(obj)
    if (expandedJson
          .get(types.`@type`)
          .map(extractIris(_))
          .exists(iris => DataType.ontology.iris.intersect(iris.toSet).nonEmpty)) {
      toNode(expandedJson - types.`@extends` - types.`@properties`, Some(DataType.ontology))
        .map { node =>
          val extendsIris = expandedJson
            .get(types.`@extends`)
            .orElse(expandedJson.get(types.rdfsSubClassOf))
            .map(extractIris(_))
          extendsIris
            .map(_.map(graph.nodes.upsert(_)))
            .map(node.addOut(Label.P.`@extends`, _))
          val propertiesIris = expandedJson
            .get(types.`@properties`)
            .map(extractIris(_))
          propertiesIris.map(_.map(graph.nodes.upsert(_))).map(_.map(node.addOut(Label.P.`@properties`, _)))
          node
        }
    } else Task.raiseError(FromJsonException("datatype is not of type '@datatype'"))
  }

  def toDatatype(iri: String)(implicit activeContext: AC): Task[DataType[Any]] =
    graph.ns.datatypes
      .get(iri)
      .flatMap(_.map(Task.now)
        .getOrElse {
          Task
            .gather(
              lspace.datatype.util.TypeHelper
                .getTypes(iri)
                ._1
                .filter(iri => ClassType.classtypes.get(iri).isEmpty)
                .map(nsDecoder.fetchClassTypeNode(_).toListL))
            .map(_.flatten)
            .flatMap { nodes =>
              val unfinished = nodes.filter(node =>
                node.hasLabel(Ontology.ontology).isEmpty && node.hasLabel(Property.ontology).isEmpty)
              Task.gather(unfinished.map(_.iri).map(toClasstype)).flatMap { clstypes =>
                CollectionType
                  .get(iri)
                  .map(Task.now)
                  .getOrElse(Task.raiseError(FromJsonException("could not build collectiontype")))
              }
            }
        })

  def toDatatypes(json: Json)(implicit activeContext: AC): Task[List[DataType[Any]]] =
    Task.sequence(extractIris(json).map(toDatatype))

  def prepareClassTypeNode(expandedJson: ExpandedMap[Json])(implicit activeContext: AC): Task[Node] = {
//    val expandedJson = activeContext.expandKeys(obj)
    val typeIris = expandedJson
      .get(types.`@type`)
      .toList
      .flatMap(extractIris(_))
      .toSet
    val iris = expandedJson
      .get(types.`@id`)
      .toList
      .flatMap(extractIris(_))
      .toSet
    if (iris.isEmpty) scribe.warn(s"no iris for ${expandedJson.obj}")
    if (Ontology.ontology.iris & typeIris nonEmpty) {
      prepareOntologyNode(expandedJson)
      //      Observable.fromTask(toNode(expandedJson, Some(Ontology.ontology))).flatMap(prepareOntology)
    } else if (Property.ontology.iris & typeIris nonEmpty) {
      preparePropertyNode(expandedJson)
      //      Observable.fromTask(toNode(expandedJson, Some(Property.ontology))).flatMap(prepareProperty)
    } else if (DataType.ontology.iris & typeIris nonEmpty) {
      prepareDataTypeNode(expandedJson)
      //      Observable.fromTask(toNode(expandedJson, Some(DataType.ontology))).flatMap(prepareDataType)
    } else {
      scribe.warn(s"preparingClassTypeNode $iris without type ${typeIris}")
      fetchClassTypeNode(iris.head).toListL.flatMap { nodes =>
        nodes
          .find(_.iri == iris.head)
          .map(Task.now)
          .getOrElse(Task.raiseError(FromJsonException("fetchClassTypeNode failed")))
      }
    }
//      Task.raiseError(FromJsonException(s"classtype is not of type '@class' ${obj}"))
  }

  def toClasstype(iri: String)(implicit activeContext: AC): Task[ClassType[Any]] =
    graph.ns.classtypes
      .get(iri)
      .flatMap(
        _.map(Task.now)
          .getOrElse(ClassType.classtypes
            .get(iri)
            .map(_.task)
            .getOrElse {
              nsDecoder.fetchClassTypeNode(iri).toListL.flatMap {
                nodes =>
                  val unfinished = nodes.filter(node =>
                    node.hasLabel(Ontology.ontology).isEmpty && node.hasLabel(Property.ontology).isEmpty)
                  Task.gather(unfinished.map(_.iri).map(toClasstype)).flatMap { clstypes =>
                    graph.ns.classtypes
                      .get(iri)
                      .flatMap(_.map(Task.now).getOrElse(Task.raiseError(
                        FromJsonException(s"could not build classtypes $iri after having done all the preparing ..."))))
                  }
              }
            }))

  def toClasstypes(json: Json)(implicit activeContext: AC): Task[List[ClassType[Any]]] =
    Task.gather(extractIris(json).map(toClasstype))

  def toList(list: List[Json], label: List[ClassType[_]])(implicit activeContext: AC): Task[List[Any]] =
    Task.gather {
      list.map { json =>
        toObject(json, label).map(_._2)
      }
    }

  def toSet(list: List[Json], label: List[ClassType[_]])(implicit activeContext: AC): Task[Set[Any]] =
    Task.gather {
      list.toSet.map { json: Json =>
        toObject(json, label).map(_._2)
      }
    }

  def toListSet(list: List[Json], label: List[ClassType[_]])(implicit activeContext: AC): Task[ListSet[Any]] =
    Task
      .gather {
        list.map { json =>
          toObject(json, label).map(_._2)
        }
      }
      .map(_.to[ListSet])

  def toVector(list: List[Json], label: List[ClassType[_]])(implicit activeContext: AC): Task[Vector[Any]] =
    Task.gather {
      list.toVector.map { json =>
        toObject(json, label).map(_._2)
      }
    }

  def toMap(list: List[Json], keyLabel: List[ClassType[_]], valueLabel: List[ClassType[_]])(
      implicit activeContext: AC): Task[Map[Any, Any]] =
    Task
      .gather {
        list.map { json =>
          json.list
            .map {
              case List(key, value) =>
                Task.parMap2(toObject(key, keyLabel).map(_._2), toObject(value, valueLabel).map(_._2))(_ -> _)
              case _ => Task.raiseError(UnexpectedJsonException("not a map structure"))
            }
            .getOrElse(Task.raiseError(UnexpectedJsonException("not a map structure")))
        }
      }
      .map(_.toMap)

  def extractOntologies(obj: ExpandedMap[Json])(implicit activeContext: AC): Task[List[Ontology]] =
    obj.get(types.`@type`).map(toOntologies(_)).getOrElse(Task.now(List()))
  def extractProperty(obj: ExpandedMap[Json])(implicit activeContext: AC): Task[Option[Property]] =
    obj.get(types.`@type`).map(toProperties(_)).map(_.map(_.headOption)).getOrElse(Task.now(None))
  def extractDatatype(obj: ExpandedMap[Json])(implicit activeContext: AC): Task[Option[DataType[Any]]] =
    obj.get(types.`@type`).map(toDatatypes(_)).map(_.map(_.headOption)).getOrElse(Task.now(None))
  def extractType(obj: ExpandedMap[Json])(implicit activeContext: AC): Task[List[ClassType[Any]]] =
    obj.get(types.`@type`).map(toClasstypes(_)).getOrElse(Task.now(List()))

  protected lazy val wip: concurrent.Map[String, Task[String]] =
    new ConcurrentHashMap[String, Task[String]](16, 0.9f, 32).asScala

  def fetchOntologyNode(iri: String)(implicit activeContext: AC): Observable[Node] = {
    Observable.fromTask(fetch(iri)).flatMap { json =>
      json.obj
        .map { obj =>
          println(s"extracting @context $iri")
          Observable.fromTask(extractContext(obj)).flatMap { implicit activeContext =>
            val expandedJson = ExpandedMap(obj)
            if (expandedJson.contains(types.`@type`))
              Observable.fromTask(prepareOntologyNode(expandedJson - types.`@context`))
            else if (expandedJson.contains(types.`@graph`)) {
              expandedJson
                .get(types.`@graph`)
                .flatMap(json =>
                  json.list.map { jsons =>
                    Observable
                      .fromIterable(jsons.map(_.obj).filter(_.exists(_.size > 2)))
                      .flatMap(_.map(obj =>
                        Observable.fromTask(extractContext(obj).flatMap(implicit activeContext =>
                          prepareClassTypeNode(ExpandedMap(obj) - types.`@context`))))
                        .getOrElse(Observable.raiseError(FromJsonException("@graph should be a list of objects"))))
                })
                .getOrElse(Observable.raiseError(FromJsonException("@graph is not an array")))
            } else
              Observable.raiseError(
                FromJsonException(s"cannot parse ontology, not @type or @graph ${expandedJson.keys}"))
          }
        }
        .getOrElse(Observable.raiseError(FromJsonException("Ontology resource is not an object")))
    }
  }

  def fetchPropertyNode(iri: String)(implicit activeContext: AC): Observable[Node] = {
    Observable.fromTask(fetch(iri)).flatMap { json =>
      json.obj
        .map { obj =>
          Observable.fromTask(extractContext(obj)).flatMap { implicit activeContext =>
            val expandedJson = ExpandedMap(obj)
            if (expandedJson.contains(types.`@type`))
              Observable.fromTask(preparePropertyNode(expandedJson - types.`@context`))
            else if (expandedJson.contains(types.`@graph`)) {
              expandedJson
                .get(types.`@graph`)
                .flatMap { json =>
                  json.list.map { jsons =>
                    Observable
                      .fromIterable(jsons
                        .map(_.obj)
                        .filter(_.exists(_.size > 2)))
                      .flatMap(_.map(obj =>
                        Observable.fromTask(extractContext(obj).flatMap(implicit activeContext =>
                          prepareClassTypeNode(ExpandedMap(obj) - types.`@context`))))
                        .getOrElse(Observable.raiseError(FromJsonException("@graph should be a list of objects"))))
                  }
                }
                .getOrElse(Observable.raiseError(FromJsonException("@graph is not an array")))
            } else
              activeContext
                .extractId(expandedJson)
                .map { iri =>
                  Observable(graph.nodes.upsert(iri, Property.unknownProperty))
                }
                .getOrElse(
                  Observable.raiseError(
                    FromJsonException(s"cannot parse property, not @type or @graph $json ${obj.keys}"))
                )
          //              Observable.raiseError(FromJsonException("cannot parse property, not @type or @graph"))
          }
        }
        //        .getOrElse(Observable.now(graph.nodes.upsert(iri, Property.unknownProperty))) ////Observable.empty[Node])
        .getOrElse(Observable.raiseError(FromJsonException("Property resource is not an object")))
    }
  }
  def fetchClassTypeNode(iri: String)(implicit activeContext: AC): Observable[Node] = {
    Observable.fromTask(fetch(iri)).flatMap { json =>
      json.obj
        .map(ExpandedMap(_))
        .map { obj =>
          Observable.fromTask(extractContext(obj.obj)).flatMap { implicit activeContext =>
            if (obj.contains(types.`@type`)) Observable.fromTask(prepareClassTypeNode(obj - types.`@context`))
            else if (obj.contains(types.`@graph`)) {
              obj
                .get(types.`@graph`)
                .flatMap { json =>
                  json.list.map { jsons =>
                    Observable
                      .fromIterable(jsons
                        .map(_.obj)
                        .filter(_.exists(_.size > 2)))
                      .flatMap(_.map(obj =>
                        Observable.fromTask(extractContext(obj).flatMap(implicit activeContext =>
                          prepareClassTypeNode(ExpandedMap(obj) - types.`@context`))))
                        .getOrElse(Observable.raiseError(FromJsonException("@graph should be a list of objects"))))
                  }
                }
                .getOrElse(Observable.raiseError(FromJsonException("@graph is not an array")))

            } else
              //              activeContext
              //                .extractId(obj)
              //                .map { iri =>
              //                  Observable.now(graph.nodes.upsert(iri))
              //                }
              //                .getOrElse(
              Observable.raiseError(FromJsonException(s"cannot parse classtype, not @type or @graph ${obj.keys}"))
          //            )
          //              Observable.raiseError(FromJsonException("cannot parse property, not @type or @graph"))
          }
        }
        .getOrElse(Observable.raiseError(FromJsonException(s"cannot parse classtype, not @type or @graph $json")))
    }
  }

  protected lazy val fetchingInProgress: concurrent.Map[String, Task[Json]] =
    new ConcurrentHashMap[String, Task[Json]](16, 0.9f, 32).asScala

  val httpClient: HttpClient = HttpClientImpl
  def fetch(iri: String): Task[Json] = { //TODO: create unique task, goal: do not fetch the same resource multiple times in parallel
    fetchingInProgress.getOrElseUpdate(
      iri, {
        val eIri = if (iri.startsWith("https://schema.org")) iri.stripSuffix(".jsonld") + ".jsonld" else iri
        if (iri.startsWith("https://schema.org")) {
          httpClient.application.ldjson
            .get(eIri)
            .flatMap(parse)
        } else
          parse(s"""{"@id": "${iri}"}""")
      }.memoizeOnSuccess.doOnFinish {
        case None =>
          import scala.concurrent.duration._
          scribe.trace(s"adding remove task, $iri is build")
          Task.delay(fetchingInProgress.remove(iri)).delayExecution(30 seconds).forkAndForget
        case Some(e) =>
          scribe.error(s"failure? : ${e.getMessage}")
          Task.now(fetchingInProgress.remove(iri))
      }.memoizeOnSuccess
    )
  }

  /**
    * https://www.w3.org/2018/jsonld-cg-reports/json-ld-api/#context-processing-algorithms
    * @param obj
    * @param activeContext
    * @return
    */
  def extractContext(obj: Map[String, Json])(implicit activeContext: AC): Task[AC] = {
    obj
      .get(types.`@context`)
      .map { json =>
        if (json == null) Seq[Json]()
        else
          json.list
            .map(_.toSeq)
            .orElse(Some(Seq(json)))
            .get
      }
      .map(_.foldLeft(Task.now(activeContext))(contextProcessing.apply))
      .getOrElse(Task.now(activeContext))
  }

  /**
    * https://www.w3.org/2018/jsonld-cg-reports/json-ld-api/#context-processing-algorithms
    */
  object contextProcessing {
    def processBase(obj: ExpandedMap[Json])(implicit activeContext: AC): Task[AC] = {
      obj
        .get(types.`@base`)
        .map(
          json =>
            json.string
              .map(activeContext.expandIri)
              .map(base => activeContext.copy(`@base` = Some(Some(base))))
              .map(Task.now)
              .getOrElse(Task.raiseError(FromJsonException(s"@base is not a string")))
        )
        .getOrElse(Task.now(activeContext))
    }
    def processVocab(obj: ExpandedMap[Json])(implicit activeContext: AC): Task[AC] = {
      obj
        .get(types.`@vocab`)
        .map(
          json =>
            json.string
              .map(activeContext.expandIri)
              .map(language => activeContext.copy(`@language` = List(language)))
              .map(Task.now)
              .getOrElse(Task.raiseError(FromJsonException(s"@vocab is not a string"))))
        .getOrElse(Task.now(activeContext))
    }
    def processLanguage(obj: ExpandedMap[Json])(implicit activeContext: AC): Task[AC] = {
      obj
        .get(types.`@language`)
        .orElse(obj.get(types.xsdLanguage))
        .map(
          json =>
            json.string
              .map(iri => activeContext.expandIri(iri))
              .map(language => activeContext.copy(`@language` = List(language)))
              .map(Task.now)
              .getOrElse(Task.raiseError(FromJsonException(s"@language is not a string"))))
        .getOrElse(Task.now(activeContext))
    }

    val apply: (Task[AC], Json) => Task[AC] = { (activeContextTask: Task[AC], json: Json) =>
      json.string
        .filter(_.nonEmpty)
        .map(iri =>
          fetch(iri).flatMap(json => Task.now(json)).flatMap { json =>
            json.obj
              .map(extractContext(_)(ActiveContext()))
              .getOrElse(Task.raiseError(FromJsonException("invalid remote context"))) //TODO parse types other than jsonld
              .flatMap(ac => activeContextTask.map(_ ++ ac))
        })
        .orElse {
          json.obj.map { obj =>
            activeContextTask
              .flatMap { implicit activeContext =>
                val expandedJson = ExpandedMap(obj)
                expandedJson
                  .get(types.`@base`)
                  .map(
                    json =>
                      json.string
                        .map(iri => activeContext.expandIri(iri))
                        .map(base => activeContext.copy(`@base` = Some(Some(base))))
                        .map(Task.now)
                        .getOrElse(Task.raiseError(FromJsonException(s"@base is not a string"))))
                  .getOrElse(Task.now(activeContext))
                  .flatMap(processBase(expandedJson)(_))
                  //            .flatMap { activeContext =>
                  //              obj.get(types.`@version`) map {
                  //                case "1.1" => //set processing mode
                  //                case _ => FromJsonException("invalid @version value")
                  //              }
                  //            }
                  .flatMap(processVocab(expandedJson)(_))
                  .flatMap(processLanguage(expandedJson)(_))
                  .flatMap { activeContext =>
                    (expandedJson - types.`@base` - types.`@vocab` - types.`@language` - types.xsdLanguage).obj
                      .foldLeft(Task.now(activeContext))(createTermDefinition.apply)
                  }
              }
          }
        }
        .getOrElse {
          Task.raiseError(FromJsonException("invalid local context"))
        }
    }
  }

  /**
    * https://www.w3.org/2018/jsonld-cg-reports/json-ld-api/#create-term-definition
    */
  object createTermDefinition {
    def processType(obj: ExpandedMap[Json])(implicit activeProperty: AP): Task[AP] = {
      implicit val activeContext = activeProperty.`@context`
      obj
        .get(types.`@type`)
        .map(extractIris(_).map(toClasstype))
        .map(Task.gather(_))
        .map(_.map { cts =>
          activeProperty.copy(`@type` = cts)
        })
        .getOrElse(Task.now(activeProperty))
    }
    def processContainer(obj: ExpandedMap[Json])(implicit activeProperty: AP): Task[AP] = {
      implicit val activeContext = activeProperty.`@context`
      obj
        .get(types.`@container`)
        .map { json =>
          json.string
            .map(activeContext.expandIri(_))
            .flatMap(`@container`.apply)
            .map(iri => activeProperty.copy(`@container` = iri :: Nil))
            .map(Task.now)
            .getOrElse(Task.raiseError(FromJsonException(s"@container is not a string")))
        }
        .getOrElse(Task.now(activeProperty))
    }
    def processReverse(obj: ExpandedMap[Json])(implicit activeContext: AC): Option[Task[AP]] = {
      obj.get(types.`@reverse`).map {
        case json if obj.contains(types.`@id`) || obj.contains(types.`@nest`) =>
          Task.raiseError(FromJsonException("invalid reverse property"))
        case json =>
          json.string
            .map { term =>
              activeContext.expandIri(term)
            }
            .map(
              key =>
                graph.ns.properties
                  .get(key)
                  .flatMap(_.map(Task.now).getOrElse {
                    toProperty(key)(activeContext)
                  })
                  .map(property => ActiveProperty(`@reverse` = true, property = property)))
            .getOrElse(Task.raiseError(FromJsonException("invalid IRI mapping")))
      }
    }
    def processId(obj: ExpandedMap[Json])(implicit activeContext: AC): Option[Task[AP]] = {
      obj.get(types.`@id`).map { json =>
        json.string
          .map { term =>
            activeContext.expandIri(term)
          }
          .map(
            key =>
              graph.ns.properties
                .get(key)
                .flatMap(_.map(Task.now).getOrElse {
                  toProperty(key)(activeContext)
                })
                .map(property => ActiveProperty(property = property)))
          .getOrElse(Task.raiseError(FromJsonException("invalid IRI mapping")))
      }
    }
    def processContext(obj: ExpandedMap[Json])(implicit activeContext: AC): Task[AC] = {
      obj
        .get(types.`@context`)
        .map(contextProcessing.apply(Task.now(activeContext), _))
        .getOrElse(Task.now(activeContext))
    }

    val apply: (Task[AC], (String, Json)) => Task[AC] = { (activeContextTask: Task[AC], kv: (String, Json)) =>
      if (kv._1.startsWith("@")) Task.raiseError(FromJsonException("keyword redefinition"))
      else {
        val json = kv._2
        activeContextTask.flatMap { implicit activeContext =>
          activeContext.expandIri(kv._1) match {
            case expKey =>
              json.string
                .map(activeContext.expandIri)
                .map(
                  key =>
                    graph.ns.properties
                      .get(key)
                      .flatMap(
                        _.map(Task.now)
                          .map(_.map(property =>
                            activeContext.copy(`@prefix` = activeContext.`@prefix` + (expKey -> key),
                                               definitions = activeContext.definitions + (expKey -> ActiveProperty(
                                                 property = property)))))
                          .getOrElse {
                            Task.now(activeContext.copy(`@prefix` = activeContext.`@prefix` + (expKey -> key)))
                          }))
                .orElse {
                  json.obj
                    .map(ExpandedMap(_))
                    .map { obj =>
                      processContext(obj)
                        .flatMap { implicit activeContext =>
                          processReverse(obj)
                            .orElse(processId(obj))
                            .getOrElse(graph.ns.properties
                              .get(expKey)
                              .flatMap(_.map(Task.now).getOrElse {
                                toProperty(expKey)(activeContext)
                              })
                              .map(property => ActiveProperty(property = property)))
                            .flatMap(processType(obj)(_))
                            .flatMap(processContainer(obj)(_))
                            .flatMap { implicit activeProperty =>
                              val tail = obj - types.`@id` - types.`@reverse` - types.`@container` - types.`@context` - types.`@nest` - types.`@nest` - types.`@type`
                              if (tail.nonEmpty)
                                Task.raiseError(FromJsonException(
                                  s"${tail.keys} are not a @id, @reverse, @container, @context, @nest, @prefix, or @type"))
                              else Task.now(activeProperty)
                            }
                        }
                        .map(ap => activeContext.copy(definitions = activeContext.definitions + (expKey -> ap)))
                    }
                }
                .getOrElse(Task.raiseError(FromJsonException(s"invalid term definition: $expKey")))
          }
        }
      }
    }
  }
}
