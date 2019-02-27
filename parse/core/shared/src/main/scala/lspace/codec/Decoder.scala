package lspace.codec

import java.time.{Instant, LocalDate, LocalTime}

import lspace.Label
import lspace.NS.types
import lspace.codec.exception.{FromJsonException, NotAcceptableException, UnexpectedJsonException}
import lspace.datatype._
import lspace.structure._
import lspace.parse.util.{HttpClient, HttpClientImpl}
import lspace.types.vector.{Geometry, Point, Polygon}
import monix.eval.{Coeval, Task}

import scala.collection.immutable.ListSet

object Decoder {
  type Aux[Json0] = Decoder { type Json = Json0 }

  def apply[Json0](graph0: Lspace)(implicit
                                   baseDecoder0: NativeTypeDecoder.Aux[Json0]): Decoder.Aux[Json0] =
    new Decoder {
      type Json = Json0
      val graph: Graph                                      = graph0
      implicit def baseDecoder: NativeTypeDecoder.Aux[Json] = baseDecoder0
    }
}
trait Decoder {
  def graph: Graph
  type Json
  implicit def baseDecoder: NativeTypeDecoder.Aux[Json]

//  implicit def decoder: Decoder = this
  type AC = ActiveContext
  type AP = ActiveProperty
  def getNewActiveContext: AC           = ActiveContext()
  def getNewActiveProperty: AP          = ActiveProperty()
  def parse(string: String): Task[Json] = baseDecoder.parse(string)

  implicit def jsonToList(json: Json): Option[List[Json]]       = baseDecoder.jsonToList(json)
  implicit def jsonToMap(json: Json): Option[Map[String, Json]] = baseDecoder.jsonToMap(json)
  implicit def jsonToString(json: Json): Option[String]         = baseDecoder.jsonToString(json)
  implicit def jsonToBoolean(json: Json): Option[Boolean]       = baseDecoder.jsonToBoolean(json)
  implicit def jsonToInt(json: Json): Option[Int]               = baseDecoder.jsonToInt(json)
  implicit def jsonToDouble(json: Json): Option[Double]         = baseDecoder.jsonToDouble(json)
  implicit def jsonToLong(json: Json): Option[Long]             = baseDecoder.jsonToLong(json)
  implicit def jsonToDateTime(json: Json): Option[Instant]      = (json: Option[String]).map(Instant.parse(_))
  implicit def jsonToDate(json: Json): Option[LocalDate]        = (json: Option[String]).map(LocalDate.parse(_))
  implicit def jsonToTime(json: Json): Option[LocalTime]        = (json: Option[String]).map(LocalTime.parse(_))

  implicit class WithDJson(json: Json) {
    def int: Option[Int]               = jsonToInt(json)
    def double: Option[Double]         = jsonToDouble(json)
    def long: Option[Long]             = jsonToLong(json)
    def datetime: Option[Instant]      = jsonToDateTime(json)
    def time: Option[LocalTime]        = jsonToTime(json)
    def date: Option[LocalDate]        = jsonToDate(json)
    def string: Option[String]         = jsonToString(json)
    def list: Option[List[Json]]       = jsonToList(json)
    def obj: Option[Map[String, Json]] = jsonToMap(json)
    def boolean: Option[Boolean]       = jsonToBoolean(json)
  }

  implicit def jsonToGeopoint(json: Json): Option[Point] =
    lspace.decode.fromGeoJson(json).toOption.collect { case point: Point => point }
  implicit def jsonToGeopolygon(json: Json): Option[Polygon] =
    lspace.decode.fromGeoJson(json).toOption.collect { case polygon: Polygon => polygon }

  def stringToLabeledNode(json: String, ontology: Ontology, activeContext: AC = getNewActiveContext): Task[Node] =
    parse(json)
      .flatMap(toLabeledNode(_, ontology, activeContext))

  def toLabeledNode(json: Json, ontology: Ontology, activeContext: AC = getNewActiveContext): Task[Node] = {
    jsonToMap(json)
      .map { obj =>
        extractContext(obj)(activeContext).flatMap { implicit activeContext =>
          val expandedJson = activeContext.expandKeys(obj)
          extractOntologies(expandedJson)(activeContext).flatMap { ontologies =>
            if (ontologies.contains(ontology)) toNode(expandedJson, None)(activeContext)
            else
              Task.raiseError(NotAcceptableException(s"cannot parse root object, expected @type ${ontology.iri}"))
          }
        }
      }
      .getOrElse(Task.raiseError(FromJsonException("root must be an object")))
  }

  def stringToNode(json: String, activeContext: AC = getNewActiveContext): Task[Node] =
    parse(json)
      .flatMap(toNode(_, activeContext))

  def toNode(json: Json, activeContext: AC = getNewActiveContext): Task[Node] = {
    jsonToMap(json)
      .map { obj =>
        extractContext(obj)(activeContext).flatMap { implicit activeContext =>
          val expandedJson = activeContext.expandKeys(obj)
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
  def toResource(obj: Map[String, Json], expectedType: Option[ClassType[_]])(
      implicit activeContext: AC): Task[Resource[Any]] =
    extractContext(obj).flatMap { implicit activeContext =>
      val expandedJson = activeContext.expandKeys(obj)
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
    jsonToMap(json)
      .map { obj =>
        if (expectedType.exists(_.isInstanceOf[GeometricType[_]]))
          toGeometric(json, expectedType.get.asInstanceOf[GeometricType[_]])
            .map(geo => graph.values.upsert(geo, expectedType.get.asInstanceOf[DataType[Any]]))
            .onErrorHandleWith { f =>
              toResource(obj, expectedType)
            } else toResource(obj, expectedType)
      }
      .getOrElse(toObject(json, expectedType.toList).map {
        case (classtype, resource: Resource[_]) => resource
        case (classtype: DataType[_], value)    => graph.values.create(value, classtype)
      })
  }

  def withEdges[T <: Resource[_]](resource: T, otherJson: Map[String, Json])(implicit activeContext: AC): Task[T] = {
    Task
      .gatherUnordered(otherJson.map {
        case (key, value) =>
          toProperty(key).map(_ -> value)
      })
      .map(_.map {
        case (property, json) =>
          jsonToMap(json)
            .map(toResource(_, Some(property)).map(resource.addOut(property, _)).map(List(_)))
            .orElse(jsonToList(json).map {
              array =>
                val edgesTask: Task[List[Edge[Any, Any]]] =
                  activeContext.properties.get(property).map(_.`@container`) match {
                    case Some(container)
                        if container.headOption.exists(
                          h => Set[`@container`](`@container`.`@list`, `@container`.`@set`).contains(h)) =>
                      Task.gatherUnordered(array.map {
                        json =>
                          jsonToMap(json)
                            .map(toResource(_, Some(property)).map(resource.addOut(property, _)))
                            .orElse(
                              jsonToList(json)
                                .map { array =>
                                  val expectedType = activeContext.expectedType(property)
                                  expectedType match {
                                    case Some(collectionType: CollectionType[_]) =>
                                      toCollection(array, collectionType).map(
                                        resource.addOut(property, collectionType, _))
                                    case _ =>
                                      Task.raiseError(FromJsonException("array found without @collection type"))
                                  }
                                }
                            )
                            .orElse {
                              for {
                                iri <- json: Option[String]
                                expectedType <- activeContext.expectedType(property).collect {
                                  case ontology: Ontology => ontology
                                }
                              } yield {
                                Task.now(graph.nodes.upsert(iri, expectedType)).map(resource.addOut(property, _))
                              }
                            }
                            .orElse(toPrimitive(json)
                              .map(v => resource.addOut(property, ClassType.valueToOntologyResource(v), v))
                              .map(Task.now))
                            .getOrElse(Task.raiseError(FromJsonException("cannot parse value")))
                      })
                    case None =>
                      val expectedType = activeContext.expectedType(property)
                      expectedType
                        .map {
                          case collectionType: CollectionType[_] =>
                            toCollection(array, collectionType)
                              .map(resource.addOut(property, collectionType, _))
                              .map(List(_))
                          case _ =>
                            Task.raiseError(
                              FromJsonException("array found without @collection type or @container:@list/@set"))
                        }
                        .getOrElse {
                          Task.raiseError(
                            FromJsonException("array found without expected @collection type or @container:@list/@set"))
                        }
                  }
                edgesTask
            })
            .getOrElse {
              val expectedType = activeContext.expectedType(property)
              (for {
                iri <- json: Option[String]
                et  <- expectedType.collect { case ontology: Ontology => ontology }
              } yield {
                Task.now(graph.nodes.upsert(iri, et)).map(v => resource.addOut(property, v))
              }).orElse {
                  expectedType.collect { case datatype: DataType[_] => datatype }.map { label =>
                    toValue(json, label).map(v => resource.addOut(property, v))
                  }
                }
                .orElse(toPrimitive(json)
                  .map(v => graph.values.create(v, ClassType.valueToOntologyResource(v)))
                  .map(v => resource.addOut(property, v))
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
    (json: Option[String])
      .map(activeContext.expandIri)
      .map(graph.nodes.upsert(_)) //TODO: add label if missing?
      .map(Task.now)
  //        .getOrElse(Task.raiseError(FromJsonException("object expected when parsing to node")))

  def toNode(expandedJson: Map[String, Json], label: Option[Ontology])(implicit activeContext: AC): Task[Node] = {
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
    (json: Option[String])
      .map(activeContext.expandIri)
      .flatMap(graph.edges.hasIri(_).headOption) //TODO: check if label == edge.key and throw exception if !=
      .map(Task.now)
  def toEdge(expandedJson: Map[String, Json], expectedType: Option[Property])(
      implicit activeContext: AC): Option[Task[Edge[Any, Any]]] = toEdge(expandedJson, expectedType.toList)
  def toEdge(expandedJson: Map[String, Json], expectedTypes: List[Property])(
      implicit activeContext: AC): Option[Task[Edge[Any, Any]]] = {
    (activeContext.extractFrom(expandedJson), activeContext.extractTo(expandedJson)) match {
      case (Some(source), Some(destination)) =>
        Some((for {
          from <- toResource(source, None)
          to   <- toResource(destination, None)
        } yield {
          extractProperties(expandedJson)
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
        (json: Option[String])
      case tpe: NumericType[_] =>
        tpe match {
          case DoubleType.datatype => json: Option[Double]
          case LongType.datatype   => json: Option[Long]
          case IntType.datatype    => json: Option[Int]
        }
      case tpe: CalendarType[_] =>
        tpe match {
          case DateTimeType.datatype  => json: Option[Instant]
          case LocalDateType.datatype => json: Option[LocalDate]
          case LocalTimeType.datatype => json: Option[LocalTime]
        }
      //        case tpe: ColorType[_] =>
      case BoolType.datatype => json: Option[Boolean]
      case _                 => None
    }).map(_.asInstanceOf[T])
      .map(Task.now)
      .getOrElse(Task.raiseError(UnexpectedJsonException(s"unknown LiteralType ${label.iri}")))

  def toStructured[T](json: Json, label: StructuredType[T])(implicit activeContext: AC): Task[T] =
    label match {
      case label: GeometricType[T] =>
        toGeometric(json, label)
      case label: CollectionType[T] =>
        jsonToList(json).map(toCollection(_, label)).getOrElse(Task.raiseError(UnexpectedJsonException(s"not a @list")))
      case label: TupleType[T] =>
        jsonToList(json).map(toTuple(_, label)).getOrElse(Task.raiseError(UnexpectedJsonException(s"not a @tuple")))
      case _ => Task.raiseError(UnexpectedJsonException(s"unknown StructuredType ${label.iri}"))
    }

  def toGeometric[T](json: Json, label: GeometricType[T])(implicit activeContext: AC): Task[T] = {
    import lspace.decode._
    (label match { //TODO: create specific parsers
      case label: GeopointType[_]   => json: Option[Point]
      case label: GeoPolygonType[_] => json: Option[Polygon]
      case _                        => None
    }).map(_.asInstanceOf[T])
      .map(Task.now)
      .getOrElse(Task.raiseError(UnexpectedJsonException(s"unknown GeometricType ${label.iri}")))
  }

//  def toCollection2[T](json: List[Json], label: CollectionType[T])(implicit activeContext: AC): Task[T] =
//    toCollection(Json.jArray(json), label)

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
      case dt: Tuple2Type[_, _] => toTuple2(json, dt._1stRange, dt._2ndRange).map(label -> _).map(_.asInstanceOf[T])
      case dt: Tuple3Type[_, _, _] =>
        toTuple3(json, dt._1stRange, dt._2ndRange, dt._3rdRange).map(label -> _).map(_.asInstanceOf[T])
      case dt: Tuple4Type[_, _, _, _] =>
        toTuple4(json, dt._1stRange, dt._2ndRange, dt._3rdRange, dt._4rdRange).map(label -> _).map(_.asInstanceOf[T])
      case _ => Task.raiseError(UnexpectedJsonException(s"unknown TupleType ${label.iri}"))
    }

  def toObject(json: Json, label: List[ClassType[_]])(implicit activeContext: AC): Task[(ClassType[Any], Any)] = {
    jsonToMap(json)
      .map { obj =>
        extractContext(obj).flatMap { implicit activeContext =>
          val expandedJson = activeContext.expandKeys(obj)
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

  def toObject(expandedJson: Map[String, Json], expectedType: Option[ClassType[_]])(
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

  def tryData(expandedJson: Map[String, Json], expectedType: Option[DataType[_]])(
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

  def tryValue(expandedJson: Map[String, Json], expectedType: Option[DataType[_]])(
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
    (json: Option[Long])
      .orElse(json: Option[Double])
      .orElse(json: Option[Int])
      .orElse(json: Option[String])

  /**
    * gets list or iris
    * @param json
    * @param activeContext
    * @return
    */
  def extractRefs(json: Json)(implicit activeContext: AC): List[String] =
    jsonToList(json)
      .map(array =>
        array.flatMap(json =>
          jsonToMap(json)
            .flatMap { obj =>
              val expandedJson = activeContext.expandKeys(obj)
              activeContext.extractId(expandedJson)
            }
            .orElse(json: Option[String])))
      .orElse((json: Option[String]).map(List(_)))
      .getOrElse(List())
      .map(activeContext.expandIri)

  def prepareOntology(obj: Map[String, Json])(implicit activeContext: AC): Task[Node] = {
    val expandedJson = activeContext.expandKeys(obj)
    if (expandedJson.get(types.`@type`).exists(json => (json: Option[String]).exists(_ == types.`@class`))) {
      for {
        node <- toNode(expandedJson, Some(Ontology.ontology))
        properties <- Task.gatherUnordered(
          node
            .out(Label.P.`@properties`)
            .collect { case node: Node => node }
            .filter(_.out(Label.P.`@label`).nonEmpty)
            .map(_.iri)
            .map { iri =>
              Property.properties.get(iri).map(_.task).getOrElse(fetch(iri)(obj => prepareProperty(obj)))
            })
        extended <- Task.gatherUnordered(
          node
            .out(Label.P.`@extends`)
            .collect { case node: Node => node }
            .filter(_.out(Label.P.`@label`).nonEmpty)
            .map(_.iri)
            .map { iri =>
              Ontology.ontologies.get(iri).map(_.task).getOrElse(fetch(iri)(obj => prepareOntology(obj)))
            })
      } yield {
        node
      }
    } else Task.raiseError(FromJsonException("ontology is not of type '@class'"))
  }
  def toOntology(obj: Map[String, Json])(implicit activeContext: AC): Task[Ontology] = {
    val expandedJson = activeContext.expandKeys(obj)
    if (expandedJson.get(types.`@type`).exists(json => (json: Option[String]).exists(_ == types.`@class`))) {
      activeContext.extractId(expandedJson) match {
        case Some(iri) =>
          graph.ns.ontologies.get(iri).flatMap { oO =>
            oO.map(Task.now).getOrElse {
              val nsDecoder = Decoder(graph.ns)
              nsDecoder.prepareOntology(expandedJson).flatMap(Ontology.ontologies.getOrBuild(_).task)
//              toNode(expandedJson, Ontology.ontology).flatMap { node =>
//                Task.gatherUnordered()
//                for {
//                  extended   <- node.out(`@extends`).map(toOntologies).getOrElse(Task.now(List()))
//                  properties <- `@properties`.map(toProperties).getOrElse(Task.now(List()))
//                } yield {}
//              }
//              Ontology.ontologies
//                .getOrBuild(iri)({
//                  val iris          = activeContext.extractIds(expandedJson)
//                  val labels        = activeContext.extractLabels(expandedJson)
//                  val comments      = activeContext.extractComments(expandedJson)
//                  val `@extends`    = expandedJson.get(types.`@extends`).orElse(expandedJson.get(types.rdfsSubClassOf))
//                  val `@properties` = expandedJson.get(types.`@properties`)
//
//                  for {
//                    extended   <- `@extends`.map(toOntologies).getOrElse(Task.now(List()))
//                    properties <- `@properties`.map(toProperties).getOrElse(Task.now(List()))
//                  } yield {
//                    Ontology._Ontology(iri)(
//                      iris.toSet,
//                      label = labels,
//                      comment = comments,
//                      _extendedClasses = () => extended,
//                      _properties = () => properties
//                    )
//                  }
//                })
            }
          }
        case None =>
          Task.raiseError(FromJsonException("an ontology without @id is not valid"))
      }
    } else Task.raiseError(FromJsonException("ontology is not of type '@class'"))
  }

  def toOntology(iri: String)(implicit activeContext: AC): Task[Ontology] =
    graph.ns.ontologies
      .get(iri)
      .flatMap(_.map(Task.now)
        .getOrElse(Ontology.ontologies.get(iri).map(_.task).getOrElse(fetchOntology(iri))))

  def toOntologies(json: Json)(implicit activeContext: AC): Task[List[Ontology]] = {
    jsonToList(json)
      .map(
        _.map(
          json =>
            jsonToMap(json)
              .map(toOntology)
              .orElse((json: Option[String])
                .map(activeContext.expandIri)
                .map(toOntology))
              .getOrElse(Task.raiseError(FromJsonException("nested arrays not allowed")))))
      .map(Task.gather(_))
      .orElse((json: Option[String])
        .map(activeContext.expandIri)
        .map(toOntology(_).map(List(_))))
      .getOrElse(Task.raiseError(FromJsonException("ontology should be a string or object")))
  }

  private val separators = Set('(', ')', '+')

  private def getTypes(iri: String): (List[String], String) = {
    iri.splitAt(iri.indexWhere(separators.contains)) match {
      case ("", iri) if iri.startsWith(")") => List()    -> iri.drop(1)
      case ("", iri)                        => List(iri) -> ""
      case (iri, tail) if tail.startsWith("(") =>
        iri match {
          case types.`@list` =>
            val (valueTypes, newTail) = getTypes(tail.drop(1))
            valueTypes -> newTail
          case types.`@listset` =>
            val (valueTypes, newTail) = getTypes(tail.drop(1))
            valueTypes -> newTail
          case types.`@set` =>
            val (valueTypes, newTail) = getTypes(tail.drop(1))
            valueTypes -> newTail
          case types.`@vector` =>
            val (valueTypes, newTail) = getTypes(tail.drop(1))
            valueTypes -> newTail
          case types.`@map` =>
            val (keyTypes, newTail) = getTypes(tail.drop(1))
            if (!newTail.startsWith("(")) throw new Exception("map without second block")
            val (valueTypes, newTail2) = getTypes(newTail.drop(1))
            (keyTypes ++ valueTypes) -> newTail2
          case types.`@tuple2` =>
            val (t1Types, newTail) = getTypes(tail.drop(1))
            if (!newTail.startsWith("(")) throw new Exception("tuple2 without second block")
            val (t2Types, newTail2) = getTypes(newTail.drop(1))
            (t1Types ++ t2Types) -> newTail2
          case types.`@tuple3` =>
            val (t1Types, newTail) = getTypes(tail.drop(1))
            if (!newTail.startsWith("(")) throw new Exception("tuple2 without second block")
            val (t2Types, newTail2) = getTypes(newTail.drop(1))
            if (!newTail2.startsWith("(")) throw new Exception("tuple3 without third block")
            val (t3Types, newTail3) = getTypes(newTail2.drop(1))
            (t1Types ++ t2Types ++ t3Types) -> newTail3
          case types.`@tuple4` =>
            val (t1Types, newTail) = getTypes(tail.drop(1))
            if (!newTail.startsWith("(")) throw new Exception("tuple2 without second block")
            val (t2Types, newTail2) = getTypes(newTail.drop(1))
            if (!newTail2.startsWith("(")) throw new Exception("tuple3 without third block")
            val (t3Types, newTail3) = getTypes(newTail2.drop(1))
            if (!newTail3.startsWith("(")) throw new Exception("tuple4 without fourth block")
            val (t4Types, newTail4) = getTypes(newTail3.drop(1))
            (t1Types ++ t2Types ++ t3Types ++ t4Types) -> newTail4
          case _ =>
            scribe.error("cannot parse : " + iri)
            throw new Exception("cannot parse : " + iri)
        }
      case (iri, tail) if tail.startsWith(")") => List(iri) -> tail.dropWhile(_ == ')')
      case (iri, tail) if tail.startsWith("+") =>
        val (tailTypes, newTail) = getTypes(tail.drop(1))
        (List(iri) ++ tailTypes) -> newTail
    }
  }

  def prepareProperty(obj: Map[String, Json])(implicit activeContext: AC): Task[Node] = {
    val expandedJson = activeContext.expandKeys(obj)
    if (expandedJson.get(types.`@type`).exists(json => (json: Option[String]).exists(_ == types.`@property`))) {
      for {
        node <- toNode(expandedJson, Some(Property.ontology))
        range <- Task.gatherUnordered(
          node
            .out(Label.P.`@range`)
            .collect { case node: Node => node }
            .filter(_.out(Label.P.`@label`).nonEmpty)
            .map(_.iri)
            .map { iri =>
              ClassType.classtypes
                .get(iri)
                .map(_.task)
                .getOrElse(
                  if (iri.startsWith("@"))
                    Task.gatherUnordered(
                      getTypes(iri)._1.map(iri =>
                        ClassType.classtypes
                          .get(iri)
                          .map(_.task)
                          .getOrElse(fetch(iri) { json =>
                            prepareClassType(json)
                          })))
                  else fetch(iri)(obj => prepareClassType(obj)))
            })
        properties <- Task.gatherUnordered(
          node
            .out(Label.P.`@properties`)
            .collect { case node: Node => node }
            .filter(_.out(Label.P.`@label`).nonEmpty)
            .map(_.iri)
            .map { iri =>
              Property.properties.get(iri).map(_.task).getOrElse(fetch(iri)(obj => prepareProperty(obj)))
            })
        extended <- Task.gatherUnordered(
          node
            .out(Label.P.`@extends`)
            .collect { case node: Node => node }
            .filter(_.out(Label.P.`@label`).nonEmpty)
            .map(_.iri)
            .map { iri =>
              Property.properties.get(iri).map(_.task).getOrElse(fetch(iri)(obj => prepareProperty(obj)))
            })
      } yield {
        node
      }
    } else Task.raiseError(FromJsonException("ontology is not of type '@class'"))
  }
  def toProperty(obj: Map[String, Json])(implicit activeContext: AC): Task[Property] = {
    val expandedJson = activeContext.expandKeys(obj)
    if (expandedJson.get(types.`@type`).exists(json => (json: Option[String]).exists(_ == types.`@property`))) {
      activeContext.extractId(expandedJson) match {
        case Some(iri) =>
          graph.ns.properties
            .get(iri)
            .flatMap(_.map(Task.now).getOrElse {
              val nsDecoder = Decoder(graph.ns)
              nsDecoder.prepareProperty(expandedJson).flatMap(Property.properties.getOrBuild(_).task)
//              Property.properties
//                .getOrConstruct(iri)({
//                  val iris          = activeContext.extractIds(expandedJson)
//                  val labels        = activeContext.extractLabels(expandedJson)
//                  val comments      = activeContext.extractComments(expandedJson)
//                  val `@range`      = expandedJson.get(types.`@range`).orElse(expandedJson.get(types.schemaRange))
//                  val `@extends`    = expandedJson.get(types.`@extends`).orElse(expandedJson.get(types.rdfsSubPropertyOf))
//                  val `@properties` = expandedJson.get(types.`@properties`)
//
//                  for {
//                    range      <- `@range`.map(toOntologies).getOrElse(Task.now(List()))
//                    extended   <- `@extends`.map(toProperties).getOrElse(Task.now(List()))
//                    properties <- `@properties`.map(toProperties).getOrElse(Task.now(List()))
//                  } yield {
//                    Property._Property(iri)(
//                      iris.toSet,
//                      label = labels,
//                      comment = comments,
//                      _range = () => range,
//                      _extendedClasses = () => extended,
//                      _properties = () => properties
//                    )
//                  }
//                }.map(Coeval.now))
//                .map(_.value())
            })
        case None =>
          Task.raiseError(FromJsonException("a property without @id is not valid"))
      }
    } else Task.raiseError(FromJsonException("property is not of type '@class'"))
  }
  def toProperty(iri: String)(implicit activeContext: AC): Task[Property] =
    graph.ns.properties
      .get(iri)
      .flatMap(_.map(Task.now)
        .getOrElse(Property.properties.get(iri).map(_.task).getOrElse(fetchProperty(iri))))

  def toProperties(json: Json)(implicit activeContext: AC): Task[List[Property]] =
    jsonToList(json)
      .map(
        _.map(
          json =>
            jsonToMap(json)
              .map(toProperty)
              .orElse((json: Option[String])
                .map(activeContext.expandIri)
                .map(toProperty))
              .getOrElse(Task.raiseError(FromJsonException("nested arrays not allowed")))))
      .map(Task.gather(_))
      .orElse((json: Option[String])
        .map(activeContext.expandIri)
        .map(toProperty(_).map(List(_))))
      .getOrElse(Task.raiseError(FromJsonException("property should be a string or object")))

  def toListType(expandedJson: Map[String, Json])(implicit activeContext: AC): Task[ListType[Any]] = {
    val `@valueRange` = expandedJson.get(ListType.keys.valueRange.iri)
    Task
      .gatherUnordered(`@valueRange`.map(toClasstypes).getOrElse(Task.now(List())) :: Nil)
      .map { l =>
        ListType(l.flatten)
      }
  }

  def toSetType(expandedJson: Map[String, Json])(implicit activeContext: AC): Task[SetType[Any]] = {
    val `@valueRange` = expandedJson.get(SetType.keys.valueRange.iri)
    Task
      .gatherUnordered(`@valueRange`.map(toClasstypes).getOrElse(Task.now(List())) :: Nil)
      .map { l =>
        SetType(l.flatten)
      }
  }

  def toListSetType(expandedJson: Map[String, Json])(implicit activeContext: AC): Task[ListSetType[Any]] = {
    val `@valueRange` = expandedJson.get(ListSetType.keys.valueRange.iri)
    Task
      .gatherUnordered(`@valueRange`.map(toClasstypes).getOrElse(Task.now(List())) :: Nil)
      .map { l =>
        ListSetType(l.flatten)
      }
  }

  def toVectorType(expandedJson: Map[String, Json])(implicit activeContext: AC): Task[VectorType[Any]] = {
    val `@valueRange` = expandedJson.get(VectorType.keys.valueRange.iri)
    Task
      .gatherUnordered(`@valueRange`.map(toClasstypes).getOrElse(Task.now(List())) :: Nil)
      .map { l =>
        VectorType(l.flatten)
      }
  }

  def toMapType(expandedJson: Map[String, Json])(implicit activeContext: AC): Task[MapType[Any, Any]] = {
    val `@keyRange`   = expandedJson.get(MapType.keys.keyRange.iri)
    val `@valueRange` = expandedJson.get(MapType.keys.valueRange.iri)
    Task
      .gatherUnordered(
        `@keyRange`.map(toClasstypes).getOrElse(Task.now(List())) ::
          `@valueRange`.map(toClasstypes).getOrElse(Task.now(List())) :: Nil)
      .map { l =>
        MapType(l(0), l(1))
      }
  }

  def toTuple2Type(expandedJson: Map[String, Json])(implicit activeContext: AC): Task[Tuple2Type[Any, Any]] = {
    val _1stRange = expandedJson.get(TupleType.keys._1stRange.iri)
    val _2ndRange = expandedJson.get(TupleType.keys._2ndRange.iri)
    Task
      .gatherUnordered(
        _1stRange.map(toClasstypes).getOrElse(Task.now(List())) ::
          _2ndRange.map(toClasstypes).getOrElse(Task.now(List())) :: Nil)
      .map { l =>
        Tuple2Type(l(0), l(1))
      }
  }

  def toTuple3Type(expandedJson: Map[String, Json])(implicit activeContext: AC): Task[Tuple3Type[Any, Any, Any]] = {
    val _1stRange = expandedJson.get(TupleType.keys._1stRange.iri)
    val _2ndRange = expandedJson.get(TupleType.keys._2ndRange.iri)
    val _3rdRange = expandedJson.get(TupleType.keys._3rdRange.iri)
    Task
      .gatherUnordered(
        _1stRange.map(toClasstypes).getOrElse(Task.now(List())) ::
          _2ndRange.map(toClasstypes).getOrElse(Task.now(List())) ::
          _3rdRange.map(toClasstypes).getOrElse(Task.now(List())) :: Nil)
      .map { l =>
        Tuple3Type(l(0), l(1), l(2))
      }
  }

  def toTuple4Type(expandedJson: Map[String, Json])(
      implicit activeContext: AC): Task[Tuple4Type[Any, Any, Any, Any]] = {
    val _1stRange = expandedJson.get(TupleType.keys._1stRange.iri)
    val _2ndRange = expandedJson.get(TupleType.keys._2ndRange.iri)
    val _3rdRange = expandedJson.get(TupleType.keys._3rdRange.iri)
    val _4rdRange = expandedJson.get(TupleType.keys._4rdRange.iri)
    Task
      .gatherUnordered(
        _1stRange.map(toClasstypes).getOrElse(Task.now(List())) ::
          _2ndRange.map(toClasstypes).getOrElse(Task.now(List())) ::
          _3rdRange.map(toClasstypes).getOrElse(Task.now(List())) ::
          _4rdRange.map(toClasstypes).getOrElse(Task.now(List())) :: Nil)
      .map { l =>
        Tuple4Type(l(0), l(1), l(2), l(3))
      }
  }

  def prepareDatatype(obj: Map[String, Json])(implicit activeContext: AC): Task[Node] = {
    val expandedJson = activeContext.expandKeys(obj)
    if (expandedJson.get(types.`@type`).exists(json => (json: Option[String]).exists(_ == types.`@datatype`))) {
      for {
        node <- toNode(expandedJson, Some(DataType.ontology))
        properties <- Task.gatherUnordered(
          node
            .out(Label.P.`@properties`)
            .collect { case node: Node => node }
            .filter(_.out(Label.P.`@label`).nonEmpty)
            .map(_.iri)
            .map { iri =>
              Property.properties.get(iri).map(_.task).getOrElse(fetch(iri)(obj => prepareProperty(obj)))
            })
        extended <- Task.gatherUnordered(
          node
            .out(Label.P.`@extends`)
            .collect { case node: Node => node }
            .filter(_.out(Label.P.`@label`).nonEmpty)
            .map(_.iri)
            .map { iri =>
              DataType.datatypes.get(iri).map(_.task).getOrElse(fetch(iri)(obj => prepareDatatype(obj)))
            })
      } yield {
        node
      }
    } else Task.raiseError(FromJsonException("ontology is not of type '@class'"))
  }
  def toDatatype(obj: Map[String, Json])(implicit activeContext: AC): Task[DataType[Any]] = {
    val expandedJson = activeContext.expandKeys(obj)
    if (expandedJson.get(types.`@type`).exists(json => json.string.exists(_ == types.`@datatype`))) {
      activeContext.extractId(expandedJson) match {
        case Some(iri) =>
          graph.ns.datatypes
            .get(iri)
            .map(_.orElse(CollectionType.get(iri).map(_.asInstanceOf[DataType[_]])))
            .flatMap(_.map(Task.now).getOrElse {
              val nsDecoder = Decoder(graph.ns)
              nsDecoder.prepareDatatype(expandedJson).flatMap(DataType.datatypes.getOrBuild(_).task)

//              val iris     = activeContext.extractIds(expandedJson)
//              val labels   = activeContext.extractLabels(expandedJson)
//              val comments = activeContext.extractComments(expandedJson)
//              val `@extends` = expandedJson
//                .get(types.`@extends`)
//                .orElse(expandedJson.get(types.rdfsSubClassOf))
//                .map(extractRefs)
//                .getOrElse(List())
//
//              val dtTask = {
//                if (`@extends`.lengthCompare(1) == 1)
//                  Task.raiseError(FromJsonException("datatype cannot extend multiple datatypes"))
//                else if (`@extends`.isEmpty) {
//                  if (iri.startsWith("@"))
//                    graph.ns.datatypes
//                      .get(iri)
//                      .flatMap(_.orElse(CollectionType.get(iri).map(_.asInstanceOf[DataType[Any]]))
//                        .map(Task.now)
//                        .getOrElse(Task.raiseError(FromJsonException("not a collection-iri"))))
//                      .map(Coeval.now(_))
//                  else Task.raiseError(FromJsonException("datatype should start with '@'"))
//                } else
//                  (`@extends`.head match {
//                    case types.`@list` =>
//                      toListType(expandedJson)
//                    case types.`@set` =>
//                      toSetType(expandedJson)
//                    case types.`@listset` =>
//                      toListSetType(expandedJson)
//                    case types.`@vector` =>
//                      toVectorType(expandedJson)
//                    case types.`@map` =>
//                      toMapType(expandedJson)
//                    case types.`@tuple2` =>
//                      toTuple2Type(expandedJson)
//                    case types.`@tuple3` =>
//                      toTuple3Type(expandedJson)
//                    case types.`@tuple4` =>
//                      toTuple4Type(expandedJson)
//                    case otherType =>
//                      graph.ns.datatypes
//                        .get(otherType)
//                        .flatMap(_.map(Task.now).getOrElse(Task.raiseError(FromJsonException("toDatatype error"))))
//                  }).map(Coeval.now)
//              }
//
//              DataType.datatypes
//                .get(iri)
//                .map(_.task)
//                .getOrElse(DataType.datatypes.getOrBuild)
            })
        case None =>
          Task.raiseError(FromJsonException("an datatype without @id is not valid"))
      }
    } else Task.raiseError(FromJsonException("datatype is not of type '@class'"))
  }

  def prepareClassType(obj: Map[String, Json])(implicit activeContext: AC): Task[Node] = {
    val expandedJson = activeContext.expandKeys(obj)
    if (expandedJson.get(types.`@type`).exists(json => (json: Option[String]).exists(_ == types.`@datatype`))) {
      for {
        node <- toNode(expandedJson, Some(Ontology.ontology))
        properties <- Task.gatherUnordered(
          node
            .out(Label.P.`@properties`)
            .collect { case node: Node => node }
            .filter(_.out(Label.P.`@label`).nonEmpty)
            .map(_.iri)
            .map(fetch(_)(obj => prepareProperty(obj))))
        extended <- Task.gatherUnordered(
          node
            .out(Label.P.`@extends`)
            .collect { case node: Node => node }
            .filter(_.out(Label.P.`@label`).nonEmpty)
            .map(_.iri)
            .map(fetch(_)(obj => prepareProperty(obj))))
      } yield {
        node
      }
    } else Task.raiseError(FromJsonException("ontology is not of type '@class'"))
  }
  def toClasstype(obj: Map[String, Json])(implicit activeContext: AC): Task[ClassType[Any]] = {
    val expandedJson = activeContext.expandKeys(obj)
    if (expandedJson.get(types.`@type`).exists(json => (json: Option[String]).exists(_ == types.`@datatype`))) {
      toDatatype(obj)
    } else if (expandedJson.get(types.`@type`).exists(json => (json: Option[String]).exists(_ == types.`@class`))) {
      toOntology(obj)
    } else if (expandedJson.get(types.`@type`).exists(json => (json: Option[String]).exists(_ == types.`@property`))) {
      toProperty(obj)
    } else {
      Task.raiseError(FromJsonException("cannot decode classtype"))
    }
  }

  def toClasstypes(json: Json)(implicit activeContext: AC): Task[List[ClassType[Any]]] = {
    jsonToList(json)
      .map(
        _.map(
          json =>
            jsonToMap(json)
              .map(toClasstype)
              .orElse(
                json.string
                  .map(activeContext.expandIri)
                  .map(
                    iri =>
                      graph.ns.classtypes.get(iri)
                        flatMap (_.map(Task.now).getOrElse(
                          CollectionType.get(iri).map(Task.now).getOrElse(fetchProperty(iri))))
                  ))
              .getOrElse(Task.raiseError(FromJsonException("nested arrays not allowed")))))
      .map(Task.gather(_))
      .orElse(
        (json: Option[String])
          .map(activeContext.expandIri)
          .map(
            iri =>
              graph.ns.classtypes.get(iri)
                flatMap (_.map(Task.now).getOrElse(CollectionType.get(iri).map(Task.now).getOrElse(fetchProperty(iri))))
          )
          .map(List(_))
          .map(Task.gather(_)))
      .getOrElse(Task.raiseError(FromJsonException("property should be a string or object")))
  }

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
          jsonToList(json)
            .map {
              case List(key, value) =>
                Task.parMap2(toObject(key, keyLabel).map(_._2), toObject(value, valueLabel).map(_._2))(_ -> _)
              case _ => Task.raiseError(UnexpectedJsonException("not a map structure"))
            }
            .getOrElse(Task.raiseError(UnexpectedJsonException("not a map structure")))
        }
      }
      .map(_.toMap)

  def toTuple2(json: List[Json], label1: List[ClassType[_]], label2: List[ClassType[_]])(
      implicit activeContext: AC): Task[(Any, Any)] =
    json match {
      case List(v1, v2) =>
        Task.parMap2(toObject(v1, label1).map(_._2), toObject(v2, label2).map(_._2))(_ -> _)
      case _ => Task.raiseError(UnexpectedJsonException("not a tuple2 structure"))
    }

  def toTuple3(json: List[Json], label1: List[ClassType[_]], label2: List[ClassType[_]], label3: List[ClassType[_]])(
      implicit activeContext: AC): Task[(Any, Any, Any)] =
    json match {
      case List(v1, v2, v3) =>
        Task.parMap3(toObject(v1, label1).map(_._2), toObject(v2, label2).map(_._2), toObject(v3, label3).map(_._2))(
          (_, _, _))
      case _ => Task.raiseError(UnexpectedJsonException("not a tuple3 structure"))
    }

  def toTuple4(json: List[Json],
               label1: List[ClassType[_]],
               label2: List[ClassType[_]],
               label3: List[ClassType[_]],
               label4: List[ClassType[_]])(implicit activeContext: AC): Task[(Any, Any, Any, Any)] =
    json match {
      case List(v1, v2, v3, v4) =>
        Task.parMap4(toObject(v1, label1).map(_._2),
                     toObject(v2, label2).map(_._2),
                     toObject(v3, label3).map(_._2),
                     toObject(v4, label4).map(_._2))((_, _, _, _))
      case _ => Task.raiseError(UnexpectedJsonException("not a tuple4 structure"))
    }

  def extractOntologies(obj: Map[String, Json])(implicit activeContext: AC): Task[List[Ontology]] =
    obj.get(types.`@type`).map(toOntologies(_)).getOrElse(Task.now(List()))
  def extractProperties(obj: Map[String, Json])(implicit activeContext: AC): Task[Option[Property]] =
    obj.get(types.`@type`).map(toProperties(_)).map(_.map(_.headOption)).getOrElse(Task.now(None))
  def extractDatatype(obj: Map[String, Json])(implicit activeContext: AC): Task[Option[DataType[Any]]] =
    obj
      .get(types.`@type`)
      .flatMap(
        json =>
          jsonToMap(json)
            .map(toDatatype(_).map(Option(_)))
            .orElse((json: Option[String]).map(graph.ns.datatypes.get(_))))
      .getOrElse(Task.now(None))

  def extractType(obj: Map[String, Json])(implicit activeContext: AC): Task[List[ClassType[Any]]] = {
    obj
      .get(types.`@type`) match {
      case Some(json) =>
        jsonToList(json) match {
          case Some(array) =>
            Task.gather(
              array.map(json =>
                jsonToMap(json)
                  .map { obj =>
                    val expandedJson = activeContext.expandKeys(obj)
                    if (expandedJson.size == 1)
                      activeContext
                        .extractId(expandedJson)
                        .map { iri =>
                          graph.ns.classtypes
                            .get(iri)
                            .flatMap(_.map(Task.now).getOrElse(
                              CollectionType.get(iri).map(Task.now).getOrElse(fetchProperty(iri))))
                        }
                        .getOrElse(throw FromJsonException("@type object without @id"))
                    else toClasstype(obj)
                  }
                  .orElse((json: Option[String]).map(activeContext.expandIri).map { iri =>
                    graph.ns.classtypes
                      .get(iri)
                      .flatMap(
                        _.map(Task.now).getOrElse(CollectionType.get(iri).map(Task.now).getOrElse(fetchProperty(iri))))
                  })
                  .getOrElse(Task.raiseError(FromJsonException("nested types not allowed")))))
          case None =>
            (json: Option[String])
              .map(activeContext.expandIri)
              .map(
                iri =>
                  graph.ns.classtypes
                    .get(iri)
                    .flatMap(
                      _.map(Task.now).getOrElse(CollectionType.get(iri).map(Task.now).getOrElse(fetchProperty(iri)))))
              .getOrElse(Task.raiseError(FromJsonException("nested types not allowed")))
              .map(List(_))
        }
      case None => Task.now(List())
    }
  }

  def fetchOntology(iri: String)(implicit activeContext: AC): Task[Ontology]      = fetch(iri)(toOntology)
  def fetchProperty(iri: String)(implicit activeContext: AC): Task[Property]      = fetch(iri)(toProperty)
  def fetchClassType(iri: String)(implicit activeContext: AC): Task[ClassType[_]] = fetch(iri)(toClasstype)

  val httpClient: HttpClient = HttpClientImpl
  def fetch[T](iri: String)(cb: Map[String, Json] => Task[T]): Task[T] = { //TODO: create unique task, goal: do not fetch the same resource multiple times in parallel
    scribe.trace(s"fetch ${iri}")
    val eIri = if (iri.startsWith("https://schema.org")) iri.stripSuffix(".jsonld") + ".jsonld" else iri
    httpClient.application.ldjson
      .get(eIri)
      .flatMap(parse)
      .map(jsonToMap(_).map(cb))
      .flatMap(_.getOrElse(throw FromJsonException("could not parse")))
  }

  def extractContext(obj: Map[String, Json])(implicit activeContext: AC): Task[AC] = {
    obj
      .get(types.`@context`)
      .map { json =>
        if (json == null) Seq[Json]()
        else
          jsonToList(json)
            .map(_.toSeq)
            .orElse(Some(Seq(json)))
            .get
      }
      .map { values =>
        values.foldLeft(Task.now(activeContext)) {
          case (activeContext, json) =>
            json.string
              .filter(_.nonEmpty)
              .map(iri => throw FromJsonException(s"remote context not yet implemented $iri"))
              .orElse {
                json.obj.map(_.toList.foldLeft(activeContext) {
                  case (activeContextTask, (key, json)) =>
                    activeContextTask.flatMap {
                      activeContext =>
                        activeContext.expandIri(key) match {
                          case types.`@base` =>
                            json.string
                              .map(iri => activeContext.expandIri(iri))
                              .map(base => activeContext.copy(`@base` = Some(base)))
                              .map(Task.now)
                              .getOrElse(Task.raiseError(FromJsonException(s"@base is not a string")))
                          case types.`@vocab` =>
                            json.string
                              .map(iri => activeContext.expandIri(iri))
                              .map(vocab => activeContext.copy(`@vocab` = Some(vocab)))
                              .map(Task.now)
                              .getOrElse(Task.raiseError(FromJsonException(s"@vocab is not a string")))
                          case types.`@language` | types.xsdLanguage =>
                            json.string
                              .map(iri => activeContext.expandIri(iri))
                              .map(language => activeContext.copy(`@language` = Some(language)))
                              .map(Task.now)
                              .getOrElse(Task.raiseError(FromJsonException(s"@language is not a string")))
                          case expKey =>
                            json.string
                              .map(activeContext.expandIri(_))
                              .map(value => activeContext.copy(`@prefix` = activeContext.`@prefix` + (expKey -> value)))
                              .map(Task.now)
                              .orElse {
                                json.obj
                                  .map { value =>
                                    graph.ns.properties
                                      .get(expKey)
                                      .flatMap(_.map(Task.now).getOrElse {
                                        fetchProperty(expKey)(activeContext)
                                      })
                                      .flatMap { property =>
                                        val newactiveContext = activeContext.copy(
                                          properties = activeContext.properties + (property -> getNewActiveProperty))
                                        value.toList.foldLeft(Task.now(newactiveContext))(bkv(property))
                                      }
                                  }
                              }
                              .getOrElse(Task.raiseError(
                                FromJsonException(s"@context/$expKey is a string nor an object")))
                        }
                    }
                })
              }
              .getOrElse {
                activeContext
              }
        }
      }
      .getOrElse(Task.now(activeContext))
  }

  private def bkv(property: Property)(activeContextTask: Task[AC], kv: (String, Json)): Task[AC] = {
    activeContextTask.flatMap { activeContext =>
      val key   = kv._1
      val value = kv._2
      val pMod  = activeContext.properties(property)
      activeContext.expandIri(key) match {
        case types.`@type` =>
          (value: Option[String])
            .map(activeContext.expandIri(_))
            .map { expKey =>
              graph.ns.classtypes
                .get(expKey)
                .flatMap(_.map(Task.now)
                  .getOrElse {
                    fetchClassType(expKey)(activeContext)
                  })
                .map { ct =>
                  activeContext.copy(properties =
                    activeContext.properties + (property -> pMod.copy(`@type` = ct :: Nil)))
                }
            }
            .orElse {
              jsonToMap(value)
                .map(toClasstype(_)(activeContext)
                  .map { ct =>
                    activeContext.copy(properties =
                      activeContext.properties + (property -> pMod.copy(`@type` = ct :: Nil)))
                  })
            }
            .getOrElse(throw FromJsonException(s"@type has unexpected value $value"))
        case types.`@vocab` =>
          (value: Option[String])
            .map(activeContext.expandIri(_))
            .map(
              iri =>
                activeContext.copy(
                  properties =
                    activeContext.properties + (property -> pMod.copy(
                      `@context` = pMod.`@context`.copy(`@vocab` = Some(iri))))))
            .map(Task.now)
            .getOrElse(Task.raiseError(FromJsonException(s"@vocab is not a string")))
        case types.`@language` | types.`@language` =>
          (value: Option[String])
            .map(activeContext.expandIri(_))
            .map(
              iri =>
                activeContext.copy(
                  properties =
                    activeContext.properties + (property -> pMod.copy(
                      `@context` = pMod.`@context`.copy(`@language` = Some(iri))))))
            .map(Task.now)
            .getOrElse(Task.raiseError(FromJsonException(s"@language is not a string")))
        case types.`@container` =>
          (value: Option[String])
            .map(activeContext.expandIri(_))
            .flatMap(`@container`.apply)
            .map(iri =>
              activeContext.copy(properties =
                activeContext.properties + (property -> pMod.copy(`@container` = iri :: Nil))))
            .map(Task.now)
            .getOrElse(Task.raiseError(FromJsonException(s"@container is not a string")))
        case types.`@base` =>
          (value: Option[String])
            .map(activeContext.expandIri(_))
            .map(
              iri =>
                activeContext.copy(
                  properties =
                    activeContext.properties + (property -> pMod.copy(
                      `@context` = pMod.`@context`.copy(`@base` = Some(iri))))))
            .map(Task.now)
            .getOrElse(Task.raiseError(FromJsonException(s"@base is not a string")))
      }
    }
  }
}
