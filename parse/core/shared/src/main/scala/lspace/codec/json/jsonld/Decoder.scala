package lspace.codec.json.jsonld

import java.util.concurrent.ConcurrentHashMap

import lspace.Label
import lspace.Label.D._
import lspace.NS.types
import lspace.codec._
import lspace.codec.exception.{FromJsonException, NotAClassNorProperty, NotAcceptableException, UnexpectedJsonException}
import lspace.codec.json.JsonDecoder
import lspace.codec.json.geojson.GeoJsonDecoder
import lspace.datatype._
import lspace.parse.util.HttpClient
import lspace.structure._
import lspace.types.string.{Blank, Iri}
import monix.eval.Task
import monix.reactive.Observable

import scala.collection.JavaConverters._
import scala.collection.concurrent
import scala.collection.immutable.{ListSet, Map}
import scala.concurrent.duration._
import scala.util.Try

object Decoder {
//  type Aux[Json0] = Decoder { type Json = Json0 }

  def apply[Json](graph0: Lspace)(implicit
                                  baseDecoder0: JsonDecoder[Json]): Decoder[Json] =
    new Decoder()(baseDecoder0) {
//      type Json = Json0
      val graph: Graph = graph0
//      implicit def baseDecoder: JsonDecoder = baseDecoder0
      lazy val nsDecoder = new Decoder()(baseDecoder0) {
//        type Json = Json0
        val graph: Graph = graph0.ns
//        implicit def baseDecoder: JsonDecoder = baseDecoder0
        lazy val nsDecoder = this
      }
    }
}

abstract class Decoder[Json](implicit val baseDecoder: JsonDecoder[Json]) extends lspace.codec.Decoder {
  def graph: Graph
  def nsDecoder: Decoder[Json]
//  type Json = baseDecoder.Json
//  implicit def baseDecoder: JsonDecoder[Json]
  implicit def geoJsonDecoder: GeoJsonDecoder[Json] = GeoJsonDecoder(baseDecoder)

  import baseDecoder._

  protected lazy val blankNodes: concurrent.Map[String, Task[Node]] =
    new ConcurrentHashMap[String, Task[Node]](16, 0.9f, 32).asScala
  protected lazy val blankEdges: concurrent.Map[String, Task[Edge[_, _]]] =
    new ConcurrentHashMap[String, Task[Edge[_, _]]](16, 0.9f, 32).asScala
  protected lazy val blankValues: concurrent.Map[String, Task[Value[_]]] =
    new ConcurrentHashMap[String, Task[Value[_]]](16, 0.9f, 32).asScala

  def apply(graph0: Lspace): Decoder[Json] = Decoder.apply(graph0)(baseDecoder)

//  implicit def decoder: Decoder = this
  def parse(string: String): Task[Json] = baseDecoder.parse(string)

  implicit class WithObj(obj: Map[String, Json]) {
    def expand(implicit activeContext: ActiveContext): ExpandedMap[Json] =
      new ExpandedMap(obj.map {
        case (key, value) => activeContext.expandIri(key).iri -> value
      })

    /**
      * https://www.w3.org/2018/jsonld-cg-reports/json-ld-api/#context-processing-algorithms
      * @param activeContext
      * @return
      */
    def extractContext(implicit activeContext: ActiveContext): Task[ActiveContext] = {
      obj
        .get(types.`@context`)
        .map { json =>
          contextProcessing.apply(activeContext, json)
//          if (json == null) Seq[Json]()
//          else
//            json.list
//              .map(_.toSeq)
//              .orElse(Some(Seq(json)))
//              .get
        }
//        .map(_.foldLeft(Task.now(activeContext))(contextProcessing.apply))
        .getOrElse(Task.now(activeContext))
    }
  }

  implicit class WithSemiExpandedMap(exp: ExpandedMap[Json]) {
    def extractId(implicit activeContext: ActiveContext) =
      exp.get(types.`@id`).flatMap(_.string).map(activeContext.expandIri)

    def extractIds(implicit activeContext: ActiveContext) =
      exp
        .get(types.`@ids`)
        .flatMap(
          json =>
            json.list
              .map(_.flatMap(_.string.orElse(throw FromJsonException("unknown key/iri format"))))
              .orElse(json.string.map(List(_))))
        .getOrElse(List())
        .map(activeContext.expandIri)

    def extractLabels(implicit activeContext: ActiveContext): Map[String, String] =
      exp
        .get(types.`@label`)
        .flatMap(
          json =>
            json.obj
              .map(_.map {
                case (key, json) =>
                  key -> json.string.getOrElse(throw FromJsonException("@label value is not a string"))
              })
              .orElse(json.string.map(l => Map("en" -> l))))
        .getOrElse(Map())

    def extractComments(implicit activeContext: ActiveContext): Map[String, String] = {
      exp
        .get(types.`@comment`)
        .flatMap(
          json =>
            json.obj
              .map(_.map {
                case (key, json) =>
                  key -> json.string.getOrElse(throw FromJsonException("@comment value is not a string"))
              })
              .orElse(json.string.map(l => Map("en" -> l))))
        .getOrElse(Map())
    }

    def extractContainer: Option[Json] =
      exp.get(types.`@container`)

    def extractValue: Option[Json] =
      exp.get(types.`@value`)

    def extractFrom: Option[Json] =
      exp.get(types.`@from`)

    def extractTo: Option[Json] =
      exp.get(types.`@to`)

    def extractOntologies(implicit activeContext: ActiveContext): Task[List[Ontology]] =
      exp.get(types.`@type`).map(toOntologies(_)).getOrElse(Task.now(List()))
    def extractProperty(implicit activeContext: ActiveContext): Task[Option[Property]] =
      exp.get(types.`@type`).map(toProperties(_)).map(_.map(_.headOption)).getOrElse(Task.now(None))
    def extractDatatype(implicit activeContext: ActiveContext): Task[Option[DataType[Any]]] =
      exp.get(types.`@type`).map(toDatatypes(_)).map(_.map(_.headOption)).getOrElse(Task.now(None))
    def extractType(implicit activeContext: ActiveContext): Task[List[ClassType[Any]]] =
      exp.get(types.`@type`).map(toClasstypes(_)).getOrElse(Task.now(List()))
  }

  def stringToLabeledNode(json: String, ontology: Ontology)(implicit activeContext: ActiveContext): Task[Node] =
    parse(json)
      .flatMap(toLabeledNode(_, ontology))

  def toLabeledNode(json: Json, ontology: Ontology)(implicit activeContext: ActiveContext): Task[Node] = {
    json.obj
      .map { obj =>
        obj.extractContext.flatMap { implicit activeContext =>
          val expandedJson = obj.expand
          expandedJson.extractOntologies.flatMap { ontologies =>
            if (ontologies.isEmpty) toNode(expandedJson, Some(ontology))
            else if (ontologies.contains(ontology) || ontologies.exists(_.`@extends`(ontology)))
              toNode(expandedJson, None)
            else
              Task.raiseError(NotAcceptableException(s"cannot parse root object, expected @type ${ontology.iri}"))
          }
        }
      }
      .getOrElse(Task.raiseError(FromJsonException("root must be an object")))
  }

  def stringToNode(json: String)(implicit activeContext: ActiveContext): Task[Node] =
    parse(json)
      .flatMap(toNode(_))

  def stringToEdge(json: String)(implicit activeContext: ActiveContext): Task[Edge[Any, Any]] =
    parse(json)
      .flatMap(toEdge(_))

  def toEdge(json: Json)(implicit activeContext: ActiveContext): Task[Edge[Any, Any]] = {
    json.obj
      .map { obj =>
        obj.extractContext.flatMap { implicit activeContext =>
          val expandedJson = obj.expand
          if (expandedJson.contains(types.`@value`))
            Task.raiseError(FromJsonException("cannot parse object with @value key to node, this looks like a value"))
          else if (expandedJson.contains(types.`@from`))
            Task.raiseError(FromJsonException("cannot parse object with @from key to node, this looks like an edge"))
          else if (expandedJson.contains(types.`@to`))
            Task.raiseError(FromJsonException("cannot parse object with @to key to node, this looks like an edge"))
          else {
            toEdge(expandedJson, None)(activeContext).get //TODO: getOrElse fail?
          }
        }
      }
      .getOrElse(Task.raiseError(FromJsonException("root must be an object")))
  }

  def toNode(json: Json)(implicit activeContext: ActiveContext): Task[Node] = {
    json.obj
      .map { obj =>
        obj.extractContext.flatMap { implicit activeContext =>
          val expandedJson = obj.expand
          if (expandedJson.contains(types.`@value`))
            Task.raiseError(FromJsonException("cannot parse object with @value key to node, this looks like a value"))
          else if (expandedJson.contains(types.`@from`))
            Task.raiseError(FromJsonException("cannot parse object with @from key to node, this looks like an edge"))
          else if (expandedJson.contains(types.`@to`))
            Task.raiseError(FromJsonException("cannot parse object with @to key to node, this looks like an edge"))
          else {
            toNode(expandedJson, None)
          }
        }
      }
      .getOrElse(Task.raiseError(FromJsonException("root must be an object")))
  }

  /**
    * @param activeContext
    * @return
    */
  def toResource(expandedJson: ExpandedMap[Json], expectedType: Option[ClassType[_]])(
      implicit activeContext: ActiveContext): Task[Resource[Any]] = {
//    extractContext(obj).flatMap { implicit activeContext =>
//      val expandedJson = activeContext.expandKeys(obj)
    expandedJson.extractType.flatMap { expectedTypes =>
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

  def toResource(json: Json, expectedType: Option[ClassType[_]])(
      implicit activeContext: ActiveContext): Task[Resource[Any]] = {
    (for {
      iri <- json.string
      et  <- expectedType.collect { case ontology: Ontology => ontology }
    } yield {
      graph.nodes.upsert(iri, et)
    }).orElse {
        json.obj
          .map { obj =>
            obj.extractContext.flatMap { implicit activeContext =>
              val expandedJson = obj.expand
              if (expectedType.exists(_.isInstanceOf[GeometricType[_]]))
                toGeometric(json, expectedType.get.asInstanceOf[GeometricType[_]])
                  .flatMap { case (label, geo) => graph.values.upsert(geo, label) }
                  .onErrorHandleWith { f =>
                    toResource(expandedJson, expectedType)
                  } else toResource(expandedJson, expectedType)
            }
          }
      }
      .getOrElse(toObject(json, expectedType).flatMap {
        case (classtype, resource: Resource[_]) => Task.now(resource)
        case (classtype: DataType[_], value)    => graph.values.create(value, classtype)
        case (classtype, value)                 => Task.raiseError(new Exception("should not happen"))
      })
  }

  /**
    * 1. map key to active property
    * 2. process value
    * 2.1 if object
    * 2.1.1 if no-container
    * 2.1.1.1 teResource
    * 2.1.2 container
    * 2.1.2.1 container-type
    * @param resource
    * @param otherJson already expanded object
    * @param activeContext
    * @tparam T
    * @return
    */
  def withEdges[T <: Resource[_]](resource: T, otherJson: ExpandedMap[Json])(
      implicit activeContext: ActiveContext): Task[T] = {
    Task
      .gatherUnordered(otherJson.obj.map {
        case (key, value) =>
          activeContext.definitions
            .get(key)
            .map(_ -> value)
            .map(Task.now)
            .getOrElse(
//              toProperty(key).map(
              Task
                .now(Property.properties.getOrCreate(key))
                .map(property =>
                  activeContext.definitions
                    .get(key)
                    .map(_ -> value)
                    .getOrElse(ActiveProperty(property = property)() -> value)))
      })
      .map(_.map {
//    Task
//      .gatherUnordered(otherJson.obj.map {
        case (activeProperty, json) =>
          val property     = activeProperty.property
          val expectedType = activeProperty.`@type`.headOption
          val addEdgeF =
            if (activeProperty.`@reverse`) (value: Resource[_]) => resource.addIn(property, value)
            else (value: Resource[_]) => resource.addOut(property, value)
          val addEdgeTypedF =
            if (activeProperty.`@reverse`)(ct: ClassType[Any], value: Any) => resource.addIn(property, ct, value)
            else (ct: ClassType[Any], value: Any) => resource.addOut(property, ct, value)
          json.obj
//            .map(_.expand) //before or after looking for @index, @language containers?
            .map {
              obj =>
                activeProperty.`@container` match {
                  case Nil =>
                    toResource(obj.expand, expectedType).flatMap(addEdgeF(_)).map(List(_))
                  case List(`@container`.`@index`) =>
                    Task.gatherUnordered(obj.toList.map { //TODO: expand keys
                      case (index, json) =>
                        toResource(json, expectedType)
                          .flatMap(addEdgeF(_))
                          .flatMap(_.addOut(Label.P.`@index`, index))
//                          .map(List(_))
                    })
                  case List(`@container`.`@language`) =>
                    Task.gatherUnordered(obj.toList.map {
                      case (language, json) =>
                        toResource(json, expectedType)
                          .flatMap(addEdgeF(_))
                          .flatMap(_.addOut(Label.P.`@language`, language))
//                          .map(List(_))
                    })
//                  case List(`@container`.`@id`) =>
//                case List(`@container`.`@type`)     =>
                  case containers =>
                    Task.raiseError(
                      FromJsonException(s"not yet supported => @container: [${containers.map(_.iri).mkString(",")}]"))
                }
            }
            .orElse(json.list.map {
              array =>
                val edgesTask: Task[List[Edge[Any, Any]]] =
                  activeProperty.`@container` match {
                    case Nil =>
                      expectedType
                        .map {
                          case collectionType: CollectionType[_] =>
                            toCollection(array, collectionType)
                              .flatMap {
                                case (collectionType, collection) => addEdgeTypedF(collectionType, collection)
                              }
                              .map(List(_))
                          case tupleType: TupleType[_] =>
                            toTuple(array, tupleType)
                              .flatMap { case (tupleType, tuple) => addEdgeTypedF(tupleType, tuple) }
                              .map(List(_))
                          case et => //no container but expected type, try ListType(List(et))
                            //                            if (property.iri == types.schemaDomainIncludes) {
//                            toCollection(array, ListType(et))
//                              .flatMap {
//                                case (cet, nodes) => Task.gatherUnordered(nodes.map(node => addEdgeTypedF(et, node)))
//                              }
                            toResource(json, expectedType).flatMap(addEdgeF(_)).map(List(_))
                          //                            } else
                          //                              Task.raiseError(FromJsonException(
                          //                                s"array found for ${property.iri} with expected type ${et.iri} in ${resource.iri} without @collection type or @container:@list/@set ${array}"))
                        }
                        .getOrElse {
                          //this processes an unexpected array as a list of edges
                          Task.gatherUnordered(array.map(toResource(_, expectedType).flatMap(addEdgeF(_))))
                          /*Task.gatherUnordered(array.map {
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
                                  .map(v => addEdgeTypedF(ClassType.detect(v), v))
                                  .map(Task.now))
                                .getOrElse(Task.raiseError(FromJsonException("cannot parse value")))
                          })*/
                          //                          Task.raiseError(FromJsonException(
                          //                            s"array found for ${property.iri} without expected @collection type or @container:@list/@set"))
                        }
                    case List(`@container`.`@list`) | List(`@container`.`@set`) =>
                      //this processes an array as a list of edges
                      Task.gatherUnordered(array.map(toResource(_, expectedType).flatMap(addEdgeF(_))))
                  }
                edgesTask
            })
            .getOrElse {
              toResource(json, expectedType).flatMap(addEdgeF(_)).map(List(_))
//              (for {
//                iri <- json.string
//                et  <- expectedType.collect { case ontology: Ontology => ontology }
//              } yield {
//                Task.now(graph.nodes.upsert(iri, et)).map(v => addEdgeF(v))
//              }).orElse {
//                  expectedType.collect { case datatype: DataType[_] => datatype }.map { label =>
//                    toValue(json, label).map(v => addEdgeF(v))
//                  }
//                }
//                .orElse(toPrimitive(json)
//                  .map(v => graph.values.create(v, ClassType.detect(v)))
//                  .map(v => addEdgeF(v))
//                  .map(Task.now(_)))
//                .getOrElse(Task.raiseError(FromJsonException("cannot parse @value")))
//                .map(List(_))
            }
        //              .getOrElse {
        //                toObject[Any](json).map {
        //                  case (label, value) =>
        //                    resource.addOut(property, label, value)
        //                }
        //              }
      })
      .flatMap { l =>
        Task.gatherUnordered(l).map { l =>
          resource
        }
      }
  }

  def tryNodeRef(json: Json)(implicit activeContext: ActiveContext): Option[Task[Node]] =
    json.string
      .map(activeContext.expandIri)
      .map {
        case Blank(iri) => blankNodes.getOrElseUpdate(iri, graph.nodes.create().memoizeOnSuccess)
        case Iri(iri)   => graph.nodes.upsert(iri)
      }
//      .map(Task.now)
  //        .getOrElse(Task.raiseError(FromJsonException("object expected when parsing to node")))

  def toNode(expandedJson: ExpandedMap[Json], label: Option[Ontology])(
      implicit activeContext: ActiveContext): Task[Node] = {
    val iri = expandedJson.extractId
    if (iri.isDefined && expandedJson.size == 1) {
      //node-ref
      iri.map {
        case Blank(iri) => blankNodes.getOrElseUpdate(iri, graph.nodes.create().memoizeOnSuccess)
        case Iri(iri)   => graph.nodes.upsert(iri)
      }.get
    } else {
      val iris = expandedJson.extractIds.toSet
      iri
        .map {
          case Blank(iri) => blankNodes.getOrElseUpdate(iri, graph.nodes.create().memoizeOnSuccess)
          case Iri(iri)   => graph.nodes.upsert(iri, iris.map(_.iri))
        }
        .getOrElse(graph.nodes.create())
        .flatMap { node =>
          expandedJson.extractOntologies.flatMap { ontologies =>
            for {
              _ <- if (ontologies.isEmpty) Task.gather(label.toList.map(node.addLabel))
              else Task.gather(ontologies.map(node.addLabel))
              _ <- withEdges(node, expandedJson - types.`@context` - types.`@id` - types.`@ids` - types.`@type`)
            } yield node
          }
        }
    }
  }
  def toScopedNode(expandedJson: ExpandedMap[Json], label: Option[Ontology])(
      implicit activeContext: ActiveContext): Task[Node] = {
    val iri = expandedJson.extractId
    if (iri.isDefined && expandedJson.size == 1) {
      //node-ref
      iri.map {
        case Blank(iri) => blankNodes.getOrElseUpdate(iri, graph.nodes.create().memoizeOnSuccess)
        case Iri(iri)   => graph.nodes.upsert(iri)
      }.get
    } else {
      val iris = expandedJson.extractIds.toSet
      iri
        .map {
          case Blank(iri) => blankNodes.getOrElseUpdate(iri, graph.nodes.create().memoizeOnSuccess)
          case Iri(iri)   => graph.nodes.upsert(iri, iris.map(_.iri))
        }
        .getOrElse(graph.nodes.create())
        .flatMap { node =>
          expandedJson.extractOntologies.flatMap { ontologies =>
            for {
              _ <- Task.gather(ontologies ++ label.toList map (node.addLabel))
              _ <- withEdges(node, expandedJson - types.`@context` - types.`@id` - types.`@ids` - types.`@type`)
            } yield node
          }
        }
    }
  }

  def tryEdgeRef(json: Json, label: Property)(implicit activeContext: ActiveContext): Option[Task[Edge[_, _]]] =
    json.string
      .map(activeContext.expandIri)
      .flatMap {
        case Blank(iri) => blankEdges.get(iri)
        case Iri(iri)   => Some(graph.edges.hasIri(iri).headL)
      }
//      .flatMap(graph.edges.hasIri(_).headOption) //TODO: check if label == edge.key and throw exception if !=
  def toEdge(expandedJson: ExpandedMap[Json], expectedType: Option[Property])(
      implicit activeContext: ActiveContext): Option[Task[Edge[Any, Any]]] = toEdge(expandedJson, expectedType.toList)
  def toEdge(expandedJson: ExpandedMap[Json], expectedTypes: List[Property])(
      implicit activeContext: ActiveContext): Option[Task[Edge[Any, Any]]] = {
    (expandedJson.extractFrom, expandedJson.extractTo) match {
      case (Some(source), Some(destination)) =>
        Some((for {
          from <- toResource(source, None)
          to   <- toResource(destination, None)
        } yield {
          expandedJson.extractProperty
            .map(_.orElse(expectedTypes.headOption))
            .flatMap {
              _.map { label =>
                from.addOut(label, to).flatMap { edge: Edge[Any, Any] =>
                  withEdges(
                    edge,
                    expandedJson - types.`@context` - types.`@id` - types.`@ids` - types.`@from` - types.`@to` - types.`@type`)
                    .map(e => edge)
                }
              }.getOrElse(Task.raiseError(FromJsonException("unexpected @type for edge object")))
            }
        }).flatten)
      case (Some(from), None) =>
        Some(Task.raiseError(FromJsonException("incomplete edge, missing @from")))
      case (None, Some(to)) =>
        Some(Task.raiseError(FromJsonException("incomplete edge, missing @from")))
      case _ => None
    }
  }
  def toScopedEdge(expandedJson: ExpandedMap[Json], expectedType: Property)(
      implicit activeContext: ActiveContext): Option[Task[Edge[Any, Any]]] =
    toScopedEdge(expandedJson, expectedType :: Nil)
  def toScopedEdge(expandedJson: ExpandedMap[Json], expectedType: Option[Property])(
      implicit activeContext: ActiveContext): Option[Task[Edge[Any, Any]]] =
    toScopedEdge(expandedJson, expectedType.toList)
  def toScopedEdge(expandedJson: ExpandedMap[Json], expectedTypes: List[Property])(
      implicit activeContext: ActiveContext): Option[Task[Edge[Any, Any]]] = {
    (expandedJson.extractFrom, expandedJson.extractTo) match {
      case (Some(source), Some(destination)) =>
        Some((for {
          from <- toResource(source, None)
          to   <- toResource(destination, None)
        } yield {
          expandedJson.extractProperty
            .map(_.orElse(expectedTypes.headOption))
            .flatMap {
              _.filter(expectedTypes.contains)
                .map { label =>
                  from.addOut(label, to).flatMap { edge: Edge[Any, Any] =>
                    withEdges(
                      edge,
                      expandedJson - types.`@context` - types.`@id` - types.`@ids` - types.`@from` - types.`@to` - types.`@type`)
                      .map(e => edge)
                  }
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

  def toLiteral[T](json: Json, label: LiteralType[T])(
      implicit activeContext: ActiveContext): Task[(LiteralType[T], T)] =
    Task {
      (label match {
        case `@string` =>
          json.string.map(`@string` -> _)
        case tpe: NumericType[_] =>
          tpe match {
            case `@double` => json.double.map(`@double` -> _)
            case `@long`   => json.long.map(`@long`     -> _)
            case `@int`    => json.int.map(`@int`       -> _)
            case `@number` =>
              json.int.map(`@int` -> _).orElse(json.long.map(`@long` -> _)).orElse(json.double.map(`@double` -> _))
          }
        case tpe: CalendarType[_] =>
          tpe match {
            case `@localdatetime` => json.localdatetime.map(`@localdatetime` -> _)
            case `@datetime`      => json.datetime.map(`@datetime`           -> _)
            case `@date`          => json.date.map(`@date`                   -> _)
            case `@time`          => json.time.map(`@time`                   -> _)
          }
        //        case tpe: ColorType[_] =>
        case BoolType.datatype => json.boolean.map(`@boolean` -> _)
        case _                 => None
      }).map(_.asInstanceOf[(LiteralType[T], T)])
    }.map(_.getOrElse(throw UnexpectedJsonException(s"unknown LiteralType ${label.iri} for $json")))

  def toStructured[T](json: Json, label: StructuredType[T])(
      implicit activeContext: ActiveContext): Task[(StructuredType[T], T)] =
    label match {
      case label: GeometricType[T] =>
        toGeometric(json, label) //label can be generic, return resulting type
      case label: CollectionType[T] =>
        json.list.map(toCollection(_, label)).getOrElse(Task.raiseError(UnexpectedJsonException(s"not a @list")))
      case label: TupleType[T] =>
        json.list.map(toTuple(_, label)).getOrElse(Task.raiseError(UnexpectedJsonException(s"not a @tuple")))
      case _ => Task.raiseError(UnexpectedJsonException(s"unknown StructuredType ${label.iri}"))
    }

  def toGeometric[T](json: Json, label: GeometricType[T])(
      implicit activeContext: ActiveContext): Task[(GeometricType[T], T)] = {
    Task {
      (label match { //TODO: create specific parsers
        case `@geopoint`        => json.list.map(geoJsonDecoder.decodePoint).map(`@geopoint`               -> _)
        case `@geomultipoint`   => json.list.map(geoJsonDecoder.decodeMultiPoint).map(`@geomultipoint`     -> _)
        case `@geoline`         => json.list.map(geoJsonDecoder.decodeLine).map(`@geoline`                 -> _)
        case `@geomultiline`    => json.list.map(geoJsonDecoder.decodeMultiLine).map(`@geomultiline`       -> _)
        case `@geopolygon`      => json.list.map(geoJsonDecoder.decodePolygon).map(`@geopolygon`           -> _)
        case `@geomultipolygon` => json.list.map(geoJsonDecoder.decodeMultiPolygon).map(`@geomultipolygon` -> _)
        case `@geomultigeo`     => json.list.map(geoJsonDecoder.decodeMultiGeometry).map(`@geomultigeo`    -> _)
        //TRY AUTODECTECT?
        case _ => None
      }).map(_.asInstanceOf[(GeometricType[T], T)])
    }.map(_.getOrElse(throw UnexpectedJsonException(s"unknown GeometricType ${label.iri}")))
  }

  def toCollection[T](json: List[Json], label: CollectionType[T])(
      implicit activeContext: ActiveContext): Task[(CollectionType[T], T)] =
    label match {
      case label: ListType[_]    => toList(json, label.valueRange).map(_.asInstanceOf[T]).map(label -> _)
      case label: VectorType[_]  => toVector(json, label.valueRange).map(_.asInstanceOf[T]).map(label -> _)
      case label: SetType[_]     => toSet(json, label.valueRange).map(_.asInstanceOf[T]).map(label -> _)
      case label: ListSetType[_] => toListSet(json, label.valueRange).map(_.asInstanceOf[T]).map(label -> _)
      case label: MapType[_]     => toMap(json, label.keyRange, label.valueRange).map(_.asInstanceOf[T]).map(label -> _)
      case _                     => Task.raiseError(UnexpectedJsonException(s"unknown CollectionType ${label.iri}"))
    }

  def toTuple[T](json: List[Json], label: TupleType[T])(
      implicit activeContext: ActiveContext): Task[(TupleType[T], T)] =
//    label match {
//      case dt: TupleType[_] =>
    if (json.size != label.rangeTypes.size)
      Task.raiseError(UnexpectedJsonException("tuple range is not equal to tuple size"))
    else
      Task
        .gather(json.zip(label.rangeTypes).map { case (json, types) => toScopedObject(json, types) })
        .map(_.map(_._2))
        .map {
          case List(a, b)                            => (a, b)
          case List(a, b, c)                         => (a, b, c)
          case List(a, b, c, d)                      => (a, b, c, d)
          case List(a, b, c, d, e)                   => (a, b, c, d, e)
          case List(a, b, c, d, e, f)                => (a, b, c, d, e, f)
          case List(a, b, c, d, e, f, g)             => (a, b, c, d, e, f, g)
          case List(a, b, c, d, e, f, g, h)          => (a, b, c, d, e, f, g, h)
          case List(a, b, c, d, e, f, g, h, i)       => (a, b, c, d, e, f, g, h, i)
          case List(a, b, c, d, e, f, g, h, i, j)    => (a, b, c, d, e, f, g, h, i, j)
          case List(a, b, c, d, e, f, g, h, i, j, k) => (a, b, c, d, e, f, g, h, i, j, k)
        }
        .map(_.asInstanceOf[T])
        .map(label -> _)
//      case _ => Task.raiseError(UnexpectedJsonException(s"unknown TupleType ${label.iri}"))
//    }

  def toObject(json: Json, label: Option[ClassType[_]] = None)(
      implicit activeContext: ActiveContext): Task[(ClassType[Any], Any)] = {
    json.obj
      .map { obj =>
        obj.extractContext.flatMap { implicit activeContext =>
          val expandedJson = obj.expand
          expandedJson.extractValue
            .map { json =>
              expandedJson.extractDatatype.map(_.orElse(label)).flatMap { labelOption =>
                labelOption
                  .map {
                    case label: DataType[_] =>
                      toData(json, label)
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
        case label: DataType[_] => Some(toData(json, label))
      })
      .orElse(tryRaw(json, label))
      .getOrElse(Task.raiseError(UnexpectedJsonException("cannot decode to value")))
  }

  def toScopedObject(json: Json, upperbound: Option[ClassType[_]] = None)(
      implicit activeContext: ActiveContext): Task[(ClassType[Any], Any)] = {
    json.obj
      .map { obj =>
        obj.extractContext.flatMap { implicit activeContext =>
          val expandedJson = obj.expand - types.`@context`
          expandedJson.extractValue
            .map { json =>
              expandedJson.extractDatatype.flatMap { labelOption =>
                labelOption match {
                  case Some(tpe) if upperbound.forall(upperbound => upperbound == tpe || tpe <:< upperbound) =>
                    toData(json, tpe)
                  case None =>
                    upperbound
                      .map {
                        case tpe: DataType[_] =>
                          toData(json, tpe)
                      }
                      .orElse(tryRaw(json))
                      .getOrElse {
                        Task.raiseError(
                          UnexpectedJsonException("cannot decode uptyped raw to valuem, auto-detection failed"))
                      }
                  case Some(tpe) =>
                    Task.raiseError(UnexpectedJsonException(
                      s"scoped type ${tpe.iri} does not satisfy the expected upperbound ${upperbound.get.iri}"))
                }
              }
            }
            .getOrElse {
              upperbound.flatMap {
                case tpe: DataType[_] =>
                  Some(toData(json, tpe))
                case tpe: Property =>
                  toScopedEdge(expandedJson, tpe).map(_.map(tpe -> _))
                case tpe: Ontology =>
                  Some(toScopedNode(expandedJson, Some(tpe)).map(tpe -> _))
              } getOrElse {
                toObject(json)(activeContext)
              }
            }
        }
      }
      .getOrElse {
        upperbound.flatMap {
          case tpe: DataType[_] =>
            Some(toData(json, tpe))
          case tpe: Property =>
            tryEdgeRef(json, tpe).map(_.map(tpe -> _))
          case tpe: Ontology =>
            tryNodeRef(json).map(_.map(tpe -> _))
        } getOrElse {
          toObject(json)(activeContext)
        }
      }
  }

  def tryRaw(json: Json, expectedType: Option[ClassType[Any]] = None)(
      implicit activeContext: ActiveContext): Option[Task[(ClassType[Any], Any)]] = {
    //is primitive
    toPrimitive(json)
      .map(v => ClassType.detect(v) -> v)
      .map(Task.now)
      .map(_.flatMap {
        case (dt: TextType[String] @unchecked, s: String) =>
          (for {
            iri <- json.string
            et  <- expectedType.collect { case ontology: Ontology => ontology }
          } yield {
            graph.nodes.upsert(iri, et).map(et -> _)
          }).getOrElse(Task.now(dt -> s))
        case (dt, v) => Task.now(dt -> v)
      })
  }

  def toObject(expandedJson: ExpandedMap[Json], expectedType: Option[ClassType[_]])(
      implicit activeContext: ActiveContext): Task[(ClassType[Any], Any)] =
    expandedJson.extractValue
      .map { json =>
        expandedJson.extractDatatype
          .map(_.orElse(expectedType))
          .flatMap {
            _.collect { case datatype: DataType[_] => datatype }
              .map { label =>
                toData(json, label)
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
      implicit activeContext: ActiveContext): Option[Task[(ClassType[Any], Any)]] = {
    expandedJson.extractValue.map { json =>
      expandedJson.extractDatatype.map(_.orElse(expectedType)).flatMap {
        _.map { label =>
          toData(json, label)
        }.getOrElse(Task.raiseError(UnexpectedJsonException("cannot parse value without type")))
      }
    }
  }

  def toData(json: Json, label: DataType[_])(implicit activeContext: ActiveContext): Task[(DataType[Any], Any)] =
    label match {
      case label: LiteralType[Any] =>
        toLiteral(json, label)
      case label: StructuredType[Any] =>
        toStructured(json, label)
      case label: IriType[_] =>
        json.string
          .map(activeContext.expandIri)
          .map(_.iri)
          .map(graph.nodes.upsert(_).map(label -> _))
//          .map(Task(_))
          .getOrElse(Task.raiseError(UnexpectedJsonException(s"unknown IriType, expected IRI/URL ${json}")))
      case _ =>
        Task.raiseError(UnexpectedJsonException(s"unknown DataType ${label.iri}"))
    }

  def tryValue(expandedJson: ExpandedMap[Json], expectedType: Option[DataType[_]])(
      implicit activeContext: ActiveContext): Option[Task[Value[Any]]] = {
    expandedJson.extractValue.map { json =>
      expandedJson.extractDatatype
        .map(_.orElse(expectedType))
        .flatMap { label =>
          label
            .map(toValue(json, _))
            .orElse(toPrimitive(json)
              .map(v => graph.values.create(v, DataType.detect(v))))
//                .map(Task.now))
            .getOrElse(Task.raiseError(FromJsonException("cannot parse @value")))
        }
    }
  }

  def toValue(json: Json, label: DataType[_])(implicit activeContext: ActiveContext): Task[Value[Any]] =
    toData(json, label).flatMap {
      case (label, v) =>
        graph.values.create(v, label)
    }

  //Int, Long, Double or String
  def toPrimitive(json: Json): Option[Any] =
    json.int
      .orElse(json.double)
      .orElse(json.long)
      .orElse(json.string)
      .orElse(json.boolean)
      .orElse(json.geo)

  /**
    * gets list or iris
    * @param json
    * @param activeContext
    * @return
    */
  def extractIris(json: Json)(implicit activeContext: ActiveContext): List[String] =
    json.list
      .map(
        array =>
          array.flatMap(json =>
            json.obj
              .flatMap { obj =>
                val expandedJson = obj.expand
                expandedJson.extractId
              }
              .map(_.iri)
              .orElse(json.string)))
      .orElse(json.string.map(List(_)))
      .orElse(json.obj
        .flatMap { obj =>
          val expandedJson = obj.expand
          expandedJson.extractId
        }
        .map(_.iri)
        .map(List(_)))
      .getOrElse(List())
      .map(activeContext.expandIri)
      .map(_.iri)
      .filter(_.nonEmpty)

  def prepareOntology(expandedJson: ExpandedMap[Json])(implicit activeContext: ActiveContext): Task[Ontology] = {
//    scribe.trace(s"prepare ontology ${obj}")
//    val expandedJson = activeContext.expandKeys(obj)

    if (expandedJson
          .get(types.`@type`)
          .map(extractIris(_))
          .exists(iris => Ontology.ontology.iris.intersect(iris.toSet).nonEmpty)) {
      val iri  = expandedJson.extractId
      val iris = expandedJson.extractIds.toSet
      iri
        .map {
//          case Blank(iri) => blankNodes.getOrElseUpdate(iri, graph.nodes.create().memoizeOnSuccess) //ontology without fqdn? local-node?
          case Iri(iri) =>
            for {
              node <- graph.nodes.upsert(iri, Ontology.ontology)
              _    <- Task.sequence((iris.map(_.iri) + iri toList).map(node.addOut(Label.P.`@ids`, _)))
            } yield node
        }
        .map(_.flatMap { node =>
          val extendsIris = expandedJson
            .get(types.`@extends`)
            .orElse(expandedJson.get(types.rdfsSubClassOf))
            .toList
            .flatMap(extractIris(_))
          val propertiesIris = expandedJson
            .get(types.`@properties`)
            .toList
            .flatMap(extractIris(_))
          for {
            extending <- Task
              .gather(
                extendsIris
                  .map(graph.nodes
                    .upsert(_, Ontology.ontology)))
            _ <- node.addOut(Label.P.`@extends`, extending)
            properties <- Task
              .gatherUnordered(propertiesIris.map(graph.nodes.upsert(_, Property.ontology)))
            _ <- Task.gatherUnordered(properties.map(node.addOut(Label.P.`@properties`, _)))
            ontology = Ontology.ontologies.getAndUpdate(node)
            _ <- (for {
              _ <- withEdges(
                node,
                expandedJson - types.`@context` - types.`@id` - types.`@ids` - types.`@type`
                  - types.`@extends` - types.rdfsSubClassOf
//                  - types.`@label` - types.rdfsLabel
//                  - types.`@comment` - types.rdfsComment
                  - types.`@properties`
              )
              _ <- Task.gatherUnordered(
                properties
                  .filter(_.hasLabel(Property.ontology).isEmpty)
                  .filter(p => p.iris.flatMap(Property.properties.get(_).toList).isEmpty)
                  .map(node =>
                    if (!pwip.contains(node.iri))
                      Task.now(Property.properties.getOrCreate(node.iri)) //toProperty(node.iri)
                    else
                      Task.unit.delayExecution(50.millis).flatMap { f =>
                        if (Property.properties.get(node.iri).nonEmpty) Task.unit
                        else Task.raiseError(FromJsonException(s"could not build ${node.iri}"))
                    }))
            } yield {
              Ontology.ontologies.getAndUpdate(node)
            }) //.startAndForget
          } yield ontology
        })
        .getOrElse(Task.raiseError(FromJsonException(s"ontology without iri $expandedJson")))
    } else Task.raiseError(FromJsonException(s"ontology is not of type '@class' ${expandedJson.obj}"))
  }

  protected lazy val owip: concurrent.Map[String, Task[Ontology]] =
    new ConcurrentHashMap[String, Task[Ontology]](16, 0.9f, 32).asScala

  def toOntology(iri: String)(implicit activeContext: ActiveContext): Task[Ontology] = {
//    println(s"toOntology ${iri}")
    graph.ns.ontologies
      .get(iri)
      .flatMap(_.map(Task.now)
        .getOrElse {
//          println(s"toOntology ${iri}")
          owip
            .getOrElseUpdate(
              iri,
              nsDecoder
                .fetchOntology(iri)
                .doOnFinish { f =>
                  Task
                    .delay {
//                      println(s"removing $iri")
                      owip.remove(iri)
                    }
                    .delayExecution(1 seconds)
                    .startAndForget
                }
                .memoizeOnSuccess
            )
        })
  }

  def toOntologies(json: Json)(implicit activeContext: ActiveContext): Task[List[Ontology]] =
    Task.gather(extractIris(json).map(toOntology))

  def prepareProperty(expandedJson: ExpandedMap[Json])(implicit activeContext: ActiveContext): Task[Property] = {
//    scribe.trace(s"prepare property $expandedJson")
//    val expandedJson = activeContext.expandKeys(obj)
    if (expandedJson
          .get(types.`@type`)
          .map(extractIris(_))
          .exists(iris => Property.ontology.iris.intersect(iris.toSet).nonEmpty)) {
      val iri  = expandedJson.extractId
      val iris = expandedJson.extractIds.toSet
      iri
        .map {
          //          case Blank(iri) => blankNodes.getOrElseUpdate(iri, graph.nodes.create().memoizeOnSuccess) //property without fqdn? local-node?
          case Iri(iri) =>
            for {
              node <- graph.nodes.upsert(iri, Property.ontology)
              _    <- Task.sequence((iris.map(_.iri) + iri toList).map(node.addOut(Label.P.`@ids`, _)))
            } yield node
        }
        .map(_.flatMap { node =>
          val extendsIris = expandedJson
            .get(types.`@extends`)
            .orElse(expandedJson.get(types.rdfsSubPropertyOf))
            .toList
            .flatMap(extractIris(_))

          val rangeIris = expandedJson
            .get(types.`@range`)
            .orElse(expandedJson.get(types.schemaRange))
            .orElse(expandedJson.get("http://schema.org/rangeIncludes"))
            .toList
            .flatMap(extractIris(_))

          val domainIncludeIris = expandedJson
            .get(types.schemaDomainIncludes)
            .orElse(expandedJson.get("http://schema.org/domainIncludes"))
            .toList
            .flatMap(extractIris(_))

          val inverseOf = expandedJson
            .get(types.schemaInverseOf)
            .orElse(expandedJson.get("http://schema.org/inverseOf"))
            .toList
            .flatMap(extractIris(_))

          val propertiesIris = expandedJson
            .get(types.`@properties`)
            .toList
            .flatMap(extractIris(_))

          for {
            extending <- Task
              .sequence(
                extendsIris
                  .map(graph.nodes
                    .upsert(_, Property.ontology)))
            _ <- node.addOut(Label.P.`@extends`, extending)
            range <- Task
              .gather(rangeIris
                .map(graph.nodes.upsert(_)))
            _ <- node.addOut(Label.P.`@range`, range)
            inverse <- Task
              .gather(inverseOf
                .map(graph.nodes.upsert(_)))
            _ <- node.addOut(Label.P.inverseOf, inverse)
            property = try {
              Property.properties.getAndUpdate(node)
            } catch {
              case e =>
                scribe.error(s"error for $iri and $iris: ${e.getMessage}")
                throw e
            }
            _ <- (for {
              _ <- withEdges(
                node,
                expandedJson - types.`@context` - types.`@id` - types.`@ids` - types.`@type`
                  - types.`@extends` - types.rdfsSubPropertyOf
//                  - types.`@label` - types.rdfsLabel
//                  - types.`@comment` - types.rdfsComment
                  - types.`@range` - types.schemaRange
                  - types.schemaDomainIncludes
                  - types.`@properties`
              )
              includedIn <- Task.gatherUnordered(domainIncludeIris
                .map(graph.nodes.upsert(_)))
              _ <- Task.gatherUnordered(includedIn.map(_.addOut(Label.P.`@properties`, node))) //.startAndForget
              properties <- Task
                .gatherUnordered(propertiesIris.map(graph.nodes.upsert(_, Property.ontology)))
              _ <- Task.gatherUnordered(properties.map(node.addOut(Label.P.`@properties`, _)))
              _ <- Task.gatherUnordered(
                properties
                  .filter(_.hasLabel(Property.ontology).isEmpty)
                  .filter(p => p.iris.flatMap(Property.properties.get(_).toList).isEmpty)
                  .map(node =>
                    if (!pwip.contains(node.iri))
                      Task.now(Property.properties.getOrCreate(node.iri)) //toProperty(node.iri)
                    else
                      Task.unit.delayExecution(50.millis).flatMap { f =>
                        if (Property.properties.get(node.iri).nonEmpty) Task.unit
                        else Task.raiseError(FromJsonException(s"could not build ${node.iri}"))
                    }))
              _ = Property.properties.getAndUpdate(node)
              _ <- Observable
                .fromIterable(includedIn)
                .map(ClassType.classtypes.getAndUpdate)
                .onErrorHandle(f => ())
                .toListL
            } yield ()) //.startAndForget
          } yield property
        })
        .getOrElse(Task.raiseError(FromJsonException(s"property without iri $expandedJson")))
    } else Task.raiseError(FromJsonException(s"property is not of type '@property' ${expandedJson.obj}"))
  }

  protected lazy val pwip: concurrent.Map[String, Task[Property]] =
    new ConcurrentHashMap[String, Task[Property]](16, 0.9f, 32).asScala

  def toProperty(iri: String)(implicit activeContext: ActiveContext): Task[Property] = {
//    println(s"toProperty ${iri}")
    graph.ns.properties
      .get(iri)
      .flatMap {
        _.map(Task.now)
          .getOrElse {
//            println(s"toProperty ${iri}")
            //            val property = Property.properties.getOrCreate(iri, Set())
            pwip
              .getOrElseUpdate(
                iri,
                nsDecoder
                  .fetchProperty(iri)
                  .doOnFinish { f =>
                    Task
                      .delay {
//                        println(s"removing $iri")
                        pwip.remove(iri)
                      }
                      .delayExecution(1 seconds)
                      .startAndForget
                  }
                  .memoizeOnSuccess
              )
          }
      }
  }

  def toProperties(json: Json)(implicit activeContext: ActiveContext): Task[List[Property]] =
    Task.gather(extractIris(json).map(toProperty))

  private val building: concurrent.Map[String, Task[Node]] =
    new ConcurrentHashMap[String, Task[Node]]().asScala

  def prepareDataType(expandedJson: ExpandedMap[Json])(implicit activeContext: ActiveContext): Task[DataType[_]] = {
//    scribe.trace(s"prepare datatype ${obj}")
//    val expandedJson = activeContext.expandKeys(obj)
    if (expandedJson
          .get(types.`@type`)
          .map(extractIris(_))
          .exists(iris => DataType.ontology.iris.intersect(iris.toSet).nonEmpty)) {
      val iri  = expandedJson.extractId
      val iris = expandedJson.extractIds.toSet
      iri
        .map {
          //          case Blank(iri) => blankNodes.getOrElseUpdate(iri, graph.nodes.create().memoizeOnSuccess) //ontology without fqdn? local-node?
          case Iri(iri) => graph.nodes.upsert(iri, iris.map(_.iri))
        }
        .map(_.flatMap { node =>
          val extendsIris = expandedJson
            .get(types.`@extends`)
            .orElse(expandedJson.get(types.rdfsSubClassOf))
            .toList
            .flatMap(extractIris(_))
          val propertiesIris = expandedJson
            .get(types.`@properties`)
            .toList
            .flatMap(extractIris(_))
          for {
            _ <- expandedJson.extractOntologies.flatMap { ontologies =>
              if (ontologies.isEmpty) node.addLabel(DataType.ontology)
              else Task.gather(ontologies.map(node.addLabel))
            }
            extending <- Task
              .gather(
                extendsIris
                  .map(graph.nodes
                    .upsert(_, DataType.ontology)))
            _ <- node.addOut(Label.P.`@extends`, extending)
            _ <- Task
              .gatherUnordered(
                extending
                  .filter(_.hasLabel(DataType.ontology).isEmpty)
                  .filter(o => o.iris.flatMap(DataType.datatypes.get(_).toList).isEmpty)
                  .map(node =>
//                    if (!owip.contains(node.iri)) toOntology(node.iri)
//                    else
                    Task.unit.delayExecution(50.millis).flatMap { f =>
                      if (DataType.datatypes.get(node.iri).nonEmpty) Task.unit
                      else Task.raiseError(FromJsonException(s"could not build ${node.iri}"))
                  }))
            properties <- Task
              .gatherUnordered(propertiesIris.map(graph.nodes.upsert(_, Property.ontology)))
            _ <- Task.gatherUnordered(properties.map(node.addOut(Label.P.`@properties`, _)))
//            _ <- withEdges(node,
//                           expandedJson.filter(types.`@label`, types.rdfsLabel, types.`@comment`, types.rdfsComment))
            datatype = DataType.datatypes.getAndUpdate(node)
            _ <- (for {
              _ <- withEdges(
                node,
                expandedJson - types.`@context` - types.`@id` - types.`@ids` - types.`@type`
                  - types.`@extends` - types.rdfsSubClassOf
//                  - types.`@label` - types.rdfsLabel
//                  - types.`@comment` - types.rdfsComment
                  - types.`@properties`
              )
              _ <- Task.gatherUnordered(
                properties
                  .filter(_.hasLabel(Property.ontology).isEmpty)
                  .filter(p => p.iris.flatMap(Property.properties.get(_).toList).isEmpty)
                  .map(node =>
                    if (!pwip.contains(node.iri))
                      Task.now(Property.properties.getOrCreate(node.iri)) //toProperty(node.iri)
                    else
                      Task.unit.delayExecution(50.millis).flatMap { f =>
                        if (Property.properties.get(node.iri).nonEmpty) Task.unit
                        else Task.raiseError(FromJsonException(s"could not build ${node.iri}"))
                    }))
            } yield {
              DataType.datatypes.getAndUpdate(node)
            }) //.startAndForget
          } yield datatype
        })
        .getOrElse(Task.raiseError(FromJsonException(s"ontology without iri $expandedJson")))
    } else Task.raiseError(FromJsonException(s"ontology is not of type '@class' ${expandedJson.obj}"))
  }

  def toDatatype(iri: String)(implicit activeContext: ActiveContext): Task[DataType[Any]] = {
//    println(s"toDatatype ${iri}")
    graph.ns.datatypes
      .get(iri)
      .flatMap(_.map(Task.now)
        .getOrElse {
//          println(s"toDatatype ${iri}")
          Task
            .gather(
              lspace.datatype.util.TypeHelper
                .getTypes(iri)
                ._1
                .filter(iri => ClassType.classtypes.get(iri).isEmpty)
                .map(nsDecoder.fetchClassType(_)))
            .flatMap { clsTypes =>
              CollectionType
                .get(iri)
                .map(Task.now)
                .getOrElse(Task.raiseError(FromJsonException("could not build collectiontype")))
            }
        })
  }

  def toDatatypes(json: Json)(implicit activeContext: ActiveContext): Task[List[DataType[Any]]] =
    Task.sequence(extractIris(json).map(toDatatype))

  protected lazy val ctwip: concurrent.Map[String, Task[ClassType[_]]] =
    new ConcurrentHashMap[String, Task[ClassType[_]]](16, 0.9f, 32).asScala

  def prepareClassType(expandedJson: ExpandedMap[Json])(implicit activeContext: ActiveContext): Task[ClassType[_]] = {
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

    if (DataType.ontology.iris & typeIris nonEmpty) {
      ctwip.getOrElseUpdate(
        iris.head,
        prepareDataType(expandedJson).flatMap { node =>
          Task.delay(ctwip.remove(iris.head)).delayExecution(5 seconds).startAndForget.map { f =>
            node
          }
        }
//          .timeout(5.seconds)
//          .onErrorHandle { f =>
//            println(iris.toString + " :: " + f.getMessage); throw f
//          }
        .memoizeOnSuccess
      )
    } else if (Ontology.ontology.iris & typeIris nonEmpty) {
      ctwip.getOrElseUpdate(
        iris.head,
        prepareOntology(expandedJson).flatMap { ontology =>
          Task.delay(ctwip.remove(iris.head)).delayExecution(5 seconds).startAndForget.map { f =>
            ontology
          }
        }
//          .timeout(5.seconds)
//          .onErrorHandle { f =>
//            println(iris.toString + " :: " + f.getMessage); throw f
//          }
        .memoizeOnSuccess
      )
    } else if (Property.ontology.iris & typeIris nonEmpty) {
      ctwip.getOrElseUpdate(
        iris.head,
        prepareProperty(expandedJson).flatMap { node =>
          Task.delay(ctwip.remove(iris.head)).delayExecution(5 seconds).startAndForget.map { f =>
            node
          }
        }
//          .timeout(5.seconds)
//          .onErrorHandle { f =>
//            println(iris.toString + " :: " + f.getMessage); throw f
//          }
        .memoizeOnSuccess
      )
    } else {
      scribe.warn(s"preparingClassTypeNode $iris without type ${typeIris}")
      if (typeIris.nonEmpty) Task.raiseError(NotAClassNorProperty(s"${iris.mkString(" aka ")}"))
      else
        ctwip.getOrElseUpdate(
          iris.head,
          fetchClassType(iris.head).memoizeOnSuccess
        )
    }
  }

  def toClasstype(iri: String)(implicit activeContext: ActiveContext): Task[ClassType[Any]] = {
//    println(s"toClassType ${iri}")
    graph.ns.classtypes
      .get(iri)
      .flatMap(_.map(Task.now)
        .getOrElse {
          lspace.datatype.util.TypeHelper
            .getTypes(iri)
            ._1 match {
            case Nil =>
              ClassType.classtypes
                .get(iri)
                .map(Task.now)
                .getOrElse {
                  //                println(s"toClassType ${iri}")
                  ctwip.getOrElseUpdate(
                    iri,
                    nsDecoder.fetchClassType(iri).memoizeOnSuccess
                  )
                }
            case iris =>
              Task
                .gather(
                  iris
                    .filter(iri => ClassType.classtypes.get(iri).isEmpty)
                    .map(nsDecoder.fetchClassType(_)))
                .flatMap { clsTypes =>
                  CollectionType
                    .get(iri)
                    .map(Task.now)
                    .getOrElse(Task.raiseError(FromJsonException("could not build collectiontype")))
                }
          }
        })
  }

  def toClasstypes(json: Json)(implicit activeContext: ActiveContext): Task[List[ClassType[Any]]] =
    Task.gather(extractIris(json).map(toClasstype))

  def toList(list: List[Json], label: Option[ClassType[_]])(implicit activeContext: ActiveContext): Task[List[Any]] =
    Task.gather {
      list.map { json =>
        toScopedObject(json, label).map(_._2)
      }
    }

  def toSet(list: List[Json], label: Option[ClassType[_]])(implicit activeContext: ActiveContext): Task[Set[Any]] =
    Task.gather {
      list.toSet.map { json: Json =>
        toScopedObject(json, label).map(_._2)
      }
    }

  def toListSet(list: List[Json], label: Option[ClassType[_]])(
      implicit activeContext: ActiveContext): Task[ListSet[Any]] =
    Task
      .gather {
        list.map { json =>
          toScopedObject(json, label).map(_._2)
        }
      }
      .map(ListSet(_: _*)) //scala 2.13 to(ListSet)

  def toVector(list: List[Json], label: Option[ClassType[_]])(
      implicit activeContext: ActiveContext): Task[Vector[Any]] =
    Task.gather {
      list.toVector.map { json =>
        toScopedObject(json, label).map(_._2)
      }
    }

  def toMap(list: List[Json], keyLabel: Option[ClassType[_]], valueLabel: Option[ClassType[_]])(
      implicit activeContext: ActiveContext): Task[Map[Any, Any]] =
    Task
      .gather {
        list.map { json =>
          json.list
            .map {
              case List(key, value) =>
                Task.parMap2(toScopedObject(key, keyLabel).map(_._2), toScopedObject(value, valueLabel).map(_._2))(
                  _ -> _)
              case _ => Task.raiseError(UnexpectedJsonException("not a map structure"))
            }
            .getOrElse(Task.raiseError(UnexpectedJsonException("not a map structure")))
        }
      }
      .map(_.toMap)

  def fetchOntology(iri: String)(implicit activeContext: ActiveContext): Task[Ontology] = {
    fetch(iri).flatMap { json =>
      json.obj
        .map { obj =>
          obj.extractContext.flatMap { implicit activeContext =>
            val expandedJson = obj.expand
            if (expandedJson.contains(types.`@type`)) {
              prepareOntology(expandedJson - types.`@context`)
            } else if (expandedJson.contains(types.`@graph`)) {
              expandedJson
                .get(types.`@graph`)
                .flatMap(json =>
                  json.list.map { jsons =>
                    Task
                      .gatherUnordered(
                        jsons
                          .map(_.obj)
                          .filter(_.exists(_.size > 2))
                          .map {
                            _.map(obj => obj.extractContext.map(_ -> obj))
                              .getOrElse(Task.raiseError(FromJsonException("@graph should be a list of objects")))
                          })
                      .flatMap {
                        list =>
                          Task
                            .gatherUnordered { //gatherUnordered seems to deadlock?
                              list.map {
                                case (ac, obj) =>
                                  val expandedJson = obj.expand(ac)
                                  expandedJson
                                    .extractId(ac)
                                    .map(_.iri)
                                    .map { iri =>
                                      prepareClassType(expandedJson - types.`@context`)
                                        .timeout(45000.millis)
                                        .onErrorHandleWith {
                                          case e: NotAClassNorProperty =>
//                                            println(s"notaclass $iri")
                                            Task.unit
                                          case e =>
//                                            println(s"error $iri")
                                            Task.raiseError(e)
                                        }
                                        .memoizeOnSuccess
                                    }
                                    .getOrElse(Task.raiseError(FromJsonException("classtype without iri")))
                              }
                            }
                      }
                      .flatMap { f =>
                        graph.ns.nodes
                          .hasIri(iri)
                          .headOptionL
                          .flatMap(_.map(n => Task(Ontology.ontologies.getAndUpdate(n)))
                            .getOrElse(Task.raiseError(
                              throw FromJsonException(s"could not find $iri after fetching and preparing"))))
                      }
                })
                .getOrElse(Task.raiseError(FromJsonException("@graph is not an array")))
            } else {
              scribe.warn(
                s"cannot fetch ontology $iri, creating by iri (it is unknown if this ontology extends others)")
              Task(Ontology.ontologies.getOrCreate(iri))
//              Task.raiseError(FromJsonException(s"cannot parse ontology, not @type or @graph ${expandedJson.keys}"))
            }
          }
        }
        .getOrElse(Task.raiseError(FromJsonException("Ontology resource is not an object")))
    }
  }

  def fetchProperty(iri: String)(implicit activeContext: ActiveContext): Task[Property] = {
    fetch(iri).flatMap { json =>
      json.obj
        .map { obj =>
          obj.extractContext.flatMap { implicit activeContext =>
            val expandedJson = obj.expand
            if (expandedJson.contains(types.`@type`)) {
              prepareProperty(expandedJson - types.`@context`)
            } else if (expandedJson.contains(types.`@graph`)) {
              expandedJson
                .get(types.`@graph`)
                .flatMap { json =>
                  json.list.map { jsons =>
                    Task
                      .gatherUnordered(
                        jsons
                          .map(_.obj)
                          .filter(_.exists(_.size > 2))
                          .map {
                            _.map(obj => obj.extractContext.map(_ -> obj))
                              .getOrElse(Task.raiseError(FromJsonException("@graph should be a list of objects")))
                          })
                      .flatMap { list =>
                        Task.gatherUnordered {
                          list.map {
                            case (ac, obj) =>
                              val expandedJson = obj.expand(ac)
                              expandedJson
                                .extractId(ac)
                                .map(_.iri)
                                .map { iri =>
                                  prepareClassType(expandedJson - types.`@context`)
                                    .timeout(15000.millis)
                                    .onErrorHandleWith {
                                      case e: NotAClassNorProperty => Task.unit
                                      case e                       => Task.raiseError(e)
                                    }
                                    .memoizeOnSuccess
                                }
                                .get
                          }
                        }
                      }
                      .flatMap { f =>
                        graph.ns.nodes
                          .hasIri(iri)
                          .headOptionL
                          .flatMap(_.map(n => Task(Property.properties.getAndUpdate(n)))
                            .getOrElse(Task.raiseError(
                              throw FromJsonException(s"could not find $iri after fetching and preparing"))))
                      }

//                      .map(_.collectFirst {
//                        case property: Property if property.iris.contains(iri) => property
//                      }.getOrElse(throw FromJsonException(s"could not find $iri after fetching and preparing")))
                  }
                }
                .getOrElse(Task.raiseError(FromJsonException("@graph is not an array")))
            } else {
              scribe.warn(s"fetching and building $iri failed, empty property created")
              Task(Property.properties.getOrCreate(iri, Set()))
            }
//              Task.raiseError(FromJsonException(s"cannot parse property, not @type or @graph ${expandedJson.keys}"))
          }
        }
        .getOrElse(Task.raiseError(FromJsonException("Property resource is not an object")))
    }
  }
  def fetchClassType(iri: String)(implicit activeContext: ActiveContext): Task[ClassType[_]] = {
    fetch(iri).flatMap { json =>
      json.obj
        .map(_.expand)
        .map { obj =>
          obj.obj.extractContext.flatMap { implicit activeContext =>
            if (obj.contains(types.`@type`)) prepareClassType(obj - types.`@context`)
            else if (obj.contains(types.`@graph`)) {
              obj
                .get(types.`@graph`)
                .flatMap { json =>
                  json.list.map { jsons =>
                    Task
                      .gatherUnordered(
                        jsons
                          .map(_.obj)
                          .filter(_.exists(_.size > 2))
                          .map {
                            _.map(obj => obj.extractContext.map(_ -> obj))
                              .getOrElse(Task.raiseError(FromJsonException("@graph should be a list of objects")))
                          })
                      .flatMap { list =>
                        Task.gatherUnordered {
                          list.map {
                            case (ac, obj) =>
                              val expandedJson = obj.expand(ac)
                              expandedJson
                                .extractId(ac)
                                .map(_.iri)
                                .map { iri =>
                                  prepareClassType(expandedJson - types.`@context`)
                                    .timeout(15000.millis)
                                    .onErrorHandleWith {
                                      case e: NotAClassNorProperty => Task.unit
                                      case e                       => Task.raiseError(e)
                                    }
                                    .memoizeOnSuccess
                                }
                                .get
                          }
                        }
                      }
                      .flatMap { f =>
                        graph.ns.nodes
                          .hasIri(iri)
                          .headOptionL
                          .flatMap(_.map(n => Task(ClassType.classtypes.getAndUpdate(n)))
                            .getOrElse(Task.raiseError(
                              throw FromJsonException(s"could not find $iri after fetching and preparing"))))
                      }
                  }
                }
                .getOrElse(Task.raiseError(FromJsonException("@graph is not an array")))

            } else
              Task.raiseError(FromJsonException(s"cannot parse classtype, not @type or @graph ${obj.obj}"))
          }
        }
        .getOrElse(Task.raiseError(FromJsonException(s"cannot parse classtype, not @type or @graph $json")))
    }
  }

  def fetchVocabularyGraph(iri: String)(implicit activeContext: ActiveContext): Task[Unit] =
    for {
      json <- fetch(iri)
      _ <- json.obj
        .map(_.expand)
        .map { obj =>
          obj.obj.extractContext.flatMap { implicit activeContext =>
            if (obj.contains(types.`@graph`)) {
              obj
                .get(types.`@graph`)
                .flatMap { json =>
                  json.list.map { jsons =>
                    Task
                      .gatherUnordered(
                        jsons
                          .map(_.obj)
                          .filter(_.exists(_.size > 2))
                          .map {
                            _.map(obj => obj.extractContext.map(_ -> obj))
                              .getOrElse(Task.raiseError(FromJsonException("@graph should be a list of objects")))
                          })
                      .flatMap { list =>
                        Task.gatherUnordered {
                          list.map {
                            case (ac, obj) =>
                              val expandedJson = obj.expand(ac)
                              expandedJson
                                .extractId(ac)
                                .map(_.iri)
                                .map { iri =>
                                  if (ClassType.classtypes.get(iri).isEmpty)
                                    prepareClassType(expandedJson - types.`@context`)
                                      .timeout(15000.millis)
                                      .onErrorHandleWith {
                                        case e: NotAClassNorProperty => Task.unit
                                        case e                       => Task.raiseError(e)
                                      }
                                      .memoizeOnSuccess
                                  else Task.unit
                                }
                                .get
                          }
                        }
                      }
                  }
                }
                .getOrElse(Task.raiseError(FromJsonException("@graph is not an array")))

            } else
              Task.raiseError(FromJsonException(s"cannot parse classtype, not @type or @graph ${obj.obj}"))
          }
        }
        .getOrElse(Task.raiseError(FromJsonException(s"cannot parse classtype, not @type or @graph $json")))
    } yield ()

  def fetchGraph(iri: String)(implicit activeContext: ActiveContext): Task[Unit] =
    for {
      json <- fetch(iri)
      _ <- json.obj
        .map(_.expand)
        .map { obj =>
          obj.obj.extractContext.flatMap { implicit activeContext =>
            if (obj.contains(types.`@graph`)) {
              obj
                .get(types.`@graph`)
                .flatMap { json =>
                  json.list.map { jsons =>
                    Task
                      .gatherUnordered(jsons
                        .map(toResource(_, None)))
                  }
                }
                .getOrElse(Task.raiseError(FromJsonException("@graph is not an array")))

            } else
              Task.raiseError(FromJsonException(s"cannot parse $iri, no @graph key found"))
          }
        }
        .getOrElse(Task.raiseError(FromJsonException(s"cannot parse $iri, no @graph key found")))
    } yield ()

  protected lazy val fetchingInProgress: concurrent.Map[String, Task[Json]] =
    new ConcurrentHashMap[String, Task[Json]](16, 0.9f, 32).asScala

  val httpClient = lspace.parse.util.HttpClientImpl
  def fetch(iri: String): Task[Json] = { //TODO: create unique task, goal: do not fetch the same resource multiple times in parallel
    fetchingInProgress.getOrElseUpdate(
      iri, {
        val eIri = if (iri.startsWith("https://schema.org")) iri.stripSuffix(".jsonld") + ".jsonld" else iri
        if (!iri.contains("example.org") && !iri.contains("example.com")) {
          httpClient.application.ldjson
            .get(eIri)
            .onErrorHandle { f =>
              scribe.warn(f.getMessage)
              s"""{"@id": "${iri}"}"""
            }
//            .executeOn(monix.execution.Scheduler.io())
            .timeout(15.second)
//            .onErrorHandle { f =>
//              println(s"url $eIri timed out after 15 seconds"); throw f
//            }
            .flatMap(parse)
        } else
          parse(s"""{"@id": "${iri}"}""")
      }.memoizeOnSuccess.doOnFinish {
        case None =>
          import scala.concurrent.duration._
//          scribe.trace(s"adding remove task, $iri is build")
          Task.delay(fetchingInProgress.remove(iri)).delayExecution(1 seconds).startAndForget
        case Some(e) =>
          e.printStackTrace()
          scribe.error(s"failure? : ${e.getMessage}")
          Task(fetchingInProgress.remove(iri))
      }.memoizeOnSuccess
    )
  }

  /**
    * https://www.w3.org/2018/jsonld-cg-reports/json-ld-api/#context-processing-algorithms
    */
  object contextProcessing {
    def processBase(obj: ExpandedMap[Json])(implicit activeContext: ActiveContext): Task[ActiveContext] = {
      obj
        .get(types.`@base`)
        .map(
          json =>
            json.string
              .map(activeContext.expandIri)
              .map(_.iri)
              .map(base => activeContext.copy(`@base` = Some(Some(base))))
              .map(Task.now)
              .getOrElse(Task.raiseError(FromJsonException(s"@base is not a string")))
        )
        .getOrElse(Task.now(activeContext))
    }
    def processVocab(obj: ExpandedMap[Json])(implicit activeContext: ActiveContext): Task[ActiveContext] = {
      obj
        .get(types.`@vocab`)
        .map(
          json =>
            json.string
              .map(activeContext.expandIri)
              .map(_.iri)
              .map(vocab => activeContext.copy(`@vocab` = List(vocab)))
              .map(Task.now)
              .getOrElse(Task.raiseError(FromJsonException(s"@vocab is not a string"))))
        .getOrElse(Task.now(activeContext))
    }
    def processLanguage(obj: ExpandedMap[Json])(implicit activeContext: ActiveContext): Task[ActiveContext] = {
      obj
        .get(types.`@language`)
        .orElse(obj.get(types.xsdLanguage))
        .map(
          json =>
            json.string
//              .map(iri => activeContext.expandIri(iri)).map(_.iri)
              .map(language => activeContext.copy(`@language` = List(language)))
              .map(Task.now)
              .getOrElse(Task.raiseError(FromJsonException(s"@language is not a string"))))
        .getOrElse(Task.now(activeContext))
    }

    def processRemoteContext(iri: String): Task[NamedActiveContext] = {
      NamedActiveContext.get(iri).map(Task.now).getOrElse {
        fetch(iri)
          .flatMap(json => Task.now(json))
          .flatMap { json =>
            json.obj
              .map(_.extractContext(ActiveContext()).map(NamedActiveContext(iri, _)))
              .getOrElse(Task.raiseError(FromJsonException("invalid remote context"))) //TODO parse types other than jsonld
          }
          .map { namedActiveContext =>
            NamedActiveContext.cache(namedActiveContext); namedActiveContext
          }
      }
    }
    def processLocalContext(obj: Map[String, Json])(implicit activeContext: ActiveContext): Task[ActiveContext] = {
      val expandedJson = obj.expand(activeContext)
      expandedJson
        .get(types.`@base`)
        .map(
          json =>
            json.string
              .map(iri => activeContext.expandIri(iri))
              .map(_.iri)
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
          val (prefixes, definitions) =
            (expandedJson - types.`@base` - types.`@vocab` - types.`@language` - types.xsdLanguage).obj
              .partition(_._2.string.isDefined)

          for {
            ac  <- prefixes.foldLeft(Task.now(activeContext))(createTermDefinition.apply)
            ac2 <- definitions.foldLeft(Task.now(ac))(createTermDefinition.apply)
          } yield ac2
        }
    }
    val apply: (ActiveContext, Json) => Task[ActiveContext] = { (activeContext: ActiveContext, json: Json) =>
      json.string
        .filter(_.nonEmpty)
        .map(processRemoteContext(_).map { remoteContext =>
          activeContext.copy(remotes = activeContext.remotes ++ List(remoteContext))
        })
        .orElse(json.obj.map(processLocalContext(_)(activeContext)))
        .orElse {
          json.list
            .map { list =>
              val activeContextTask = list match {
                case List(first, second, _*) if first.isNull => Task.now(ActiveContext())
                case jsons                                   => Task.now(activeContext)
              }
              Observable
                .fromIterable(list)
                .foldLeftL(activeContextTask) {
                  case (activeContextTask, json) =>
                    json.string
                      .map(processRemoteContext(_).flatMap { remoteContext =>
                        activeContextTask.map(activeContext =>
                          activeContext.copy(remotes = activeContext.remotes ++ List(remoteContext)))
                      })
                      .getOrElse {
                        for {
                          activeContext <- activeContextTask
                          result <- json.obj
                            .map(processLocalContext(_)(activeContext))
                            .getOrElse(Task.raiseError(FromJsonException(s"cannot parse context $json")))
                        } yield result
                      }
                }
                .flatten
            }
        }
        .getOrElse(if (json.isNull) Task.now(ActiveContext())
        else Task.raiseError(FromJsonException(s"cannot parse context $json")))
    //            Task.raiseError(FromJsonException("invalid local context"))
    }
  }

  /**
    * https://www.w3.org/2018/jsonld-cg-reports/json-ld-api/#create-term-definition
    */
  object createTermDefinition {
    def processType(obj: ExpandedMap[Json])(implicit activeProperty: ActiveProperty): Task[ActiveProperty] = {
      implicit val activeContext = activeProperty.`@context`
      obj
        .get(types.`@type`)
        .map(extractIris(_).map(toClasstype))
        .map(Task.gather(_))
        .map(_.map { cts =>
          activeProperty.copy(`@type` = cts)()
        })
        .getOrElse(Task.now(activeProperty))
    }
    def processContainer(obj: ExpandedMap[Json])(implicit activeProperty: ActiveProperty): Task[ActiveProperty] = {
      implicit val activeContext = activeProperty.`@context`
      obj
        .get(types.`@container`)
        .map(
          json =>
            json.list
              .getOrElse(List(json))
              .map { json =>
                json.string
                  .map(activeContext.expandIri(_))
                  .map(_.iri)
                  .flatMap(`@container`.apply)
                  .map(Task.now)
                  .getOrElse(Task.raiseError(FromJsonException(s"unknown @container-type")))
            })
        .map(
          Task.gather(_).map(iris => activeProperty.copy(`@container` = iris)())
        )
        .getOrElse(Task.now(activeProperty))
    }
    def processReverse(obj: ExpandedMap[Json])(implicit activeContext: ActiveContext): Option[Task[ActiveProperty]] = {
      obj.get(types.`@reverse`).map {
        case json if obj.contains(types.`@id`) || obj.contains(types.`@nest`) =>
          Task.raiseError(FromJsonException("invalid reverse property"))
        case json =>
          json.string
            .map { term =>
              activeContext.expandIri(term)
            }
            .map(_.iri)
            .map(
              key =>
                graph.ns.properties
                  .get(key)
                  .flatMap(_.map(Task.now).getOrElse {
//                    toProperty(key)(activeContext)
                    Task
                      .now(Property.properties.getOrCreate(key))
                  })
                  .map(property => ActiveProperty(property, `@reverse` = true)()))
            .getOrElse(Task.raiseError(FromJsonException("invalid IRI mapping")))
      }
    }
    def processId(obj: ExpandedMap[Json])(implicit activeContext: ActiveContext): Option[Task[ActiveProperty]] = {
      obj.get(types.`@id`).map { json =>
        json.string
          .map { term =>
            activeContext.expandIri(term)
          }
          .map(_.iri)
          .map(
            key =>
              graph.ns.properties
                .get(key)
                .flatMap(_.map(Task.now).getOrElse {
//                  toProperty(key)(activeContext)
                  Task
                    .now(Property.properties.getOrCreate(key))
                })
                .map(property => ActiveProperty(property)()))
          .getOrElse(Task.raiseError(FromJsonException("invalid IRI mapping")))
      }
    }
    def processContext(obj: ExpandedMap[Json])(implicit activeContext: ActiveContext): Task[ActiveContext] = {
      obj
        .get(types.`@context`)
        .map(contextProcessing.apply(activeContext, _))
        .getOrElse(Task.now(activeContext))
    }

    val apply: (Task[ActiveContext], (String, Json)) => Task[ActiveContext] = {
      (activeContextTask: Task[ActiveContext], kv: (String, Json)) =>
        if (kv._1.startsWith("@")) Task.raiseError(FromJsonException("keyword redefinition"))
        else {
          val json = kv._2
          activeContextTask.flatMap { implicit activeContext =>
            activeContext.expandIri(kv._1).iri match {
              case expKey =>
                json.string
                  .map(activeContext.expandIri)
                  .map(_.iri)
                  .map(
                    key =>
                      graph.ns.properties
                        .get(key)
                        .flatMap(
                          _.map(Task.now)
                            .map(_.map(property =>
                              activeContext.copy( //`@prefix` = activeContext.`@prefix`() + (expKey -> key),
                                definitions = activeContext.definitions() + (expKey -> ActiveProperty(property)()))))
                            .getOrElse {
                              Task.now(activeContext.copy(`@prefix` = activeContext.`@prefix`() + (expKey -> key)))
                            }))
                  .orElse {
                    json.obj
                      .map(_.expand)
                      .map { obj =>
                        processContext(obj)
                          .flatMap { implicit activeContext =>
                            processReverse(obj)
                              .orElse(processId(obj))
                              .getOrElse(graph.ns.properties
                                .get(expKey)
                                .flatMap(_.map(Task.now).getOrElse {
//                                  toProperty(expKey)(activeContext)
                                  Task
                                    .now(Property.properties.getOrCreate(expKey))
                                })
                                .map(property => ActiveProperty(property)()))
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
                          .map(ap => activeContext.copy(definitions = activeContext.definitions() + (expKey -> ap)))
                      }
                  }
                  .getOrElse(Task.raiseError(FromJsonException(s"invalid term definition: $expKey")))
            }
          }
        }
    }
  }
}
