package lspace.parse

import java.time.{Instant, LocalDate, LocalTime}

import argonaut._
import Argonaut._
import lspace.NS.types
import lspace.librarian.datatype._
import lspace.librarian.structure._
import lspace.parse.util._
import lspace.types.vector.{Geometry, Point, Polygon}
import monix.eval.Task
import squants.time.Time

import scala.collection.immutable.{ListMap, ListSet}

object JsonObjectInProgress {
  def apply(json: JsonObject, doubledefinitionWA: Int = 1)(
      implicit activeContext: ActiveContext): JsonObjectInProgress =
    JsonObjectInProgress(json, activeContext)

  implicit class WithJsonObject(jip: JsonObjectInProgress) {
    lazy val withContext: JsonObject = _context(jip.activeContext) match {
      case Some(context) => JsonObject.fromTraversableOnce(ListMap(types.`@context` -> context) ++ jip.json.toMap)
      case None          => jip.json
    }
    lazy val withContextAsJson: Json = Json.jObject(withContext)

    implicit class WithIriString(iri: String)(implicit activeContext: ActiveContext) {
      lazy val compact: String = activeContext.compactIri(iri)
    }

    private def _context(activeContext: ActiveContext): Option[Json] = {
      val (newActiveContext, result) = activeContext.properties.foldLeft((activeContext, ListMap[String, Json]())) {
        case ((activeContext, result), (key, activeProperty)) =>
          val (keyIri, newActiveContext) = activeContext.compactIri(key)
          List(
            _context(activeProperty.`@context`).map(types.`@context` -> _),
            _containers(activeProperty).map(types.`@container`       -> _),
            _types(activeProperty).map(types.`@type`                 -> _)
          ).flatten match {
            case kv if kv.nonEmpty =>
              newActiveContext -> (result ++ ListMap(
                keyIri -> Json.jObject(JsonObject.fromTraversableOnce(ListMap(kv: _*)))))
            case kv =>
              newActiveContext -> result
          }
      }
      JsonObject.fromTraversableOnce(newActiveContext.`@prefix`.map {
        case (iri, prefix) => prefix -> iri.asJson
      }.toList ++ result.toList) match {
        case kv if kv.isNotEmpty => Some(Json.jObject(kv))
        case kv                  => None
      }
    }

    private def _containers(activeProperty: ActiveProperty): Option[Json] = {
      implicit val activeContext = activeProperty.`@context`
      activeProperty.`@container` match {
        case Nil             => None
        case List(container) => Some(container.iri.compact.asJson)
        case containers =>
          Some(activeProperty.`@container`.foldLeft(List[Json]()) {
            case (result, container) => result :+ container.iri.compact.asJson
          }.asJson)
      }
    }

    private def _types(activeProperty: ActiveProperty): Option[Json] = {
      implicit val activeContext = activeProperty.`@context`
      activeProperty.`@type` match {
        case Nil       => None
        case List(tpe) => Some(tpe.iri.compact.asJson)
        case tpes =>
          Some(activeProperty.`@type`.foldLeft(List[Json]()) {
            case (result, container) => result :+ container.iri.compact.asJson
          }.asJson)
      }
    }
  }
}
case class JsonObjectInProgress(json: JsonObject, activeContext: ActiveContext) {
  def map(cb: (JsonObject, ActiveContext) => JsonObjectInProgress): JsonObjectInProgress =
    cb(json, activeContext)
  def map(cb: (JsonObject, ActiveContext) => JsonInProgress): JsonInProgress =
    cb(json, activeContext)
}
object JsonInProgress {
  def apply(json: Json, doubledefinitionWA: Int = 1)(implicit activeContext: ActiveContext): JsonInProgress =
    JsonInProgress(json, activeContext)

  implicit class WithJsonInProgress(jip: JsonInProgress) {
    lazy val withContext: JsonObject =
      JsonObjectInProgress(JsonObject.single(types.`@graph`, jip.json), jip.activeContext).withContext
  }
}
case class JsonInProgress(json: Json, activeContext: ActiveContext) {
  def map(cb: (Json, ActiveContext) => JsonObjectInProgress): JsonObjectInProgress =
    cb(json, activeContext)
  def map(cb: (Json, ActiveContext) => JsonInProgress): JsonInProgress =
    cb(json, activeContext)
}

object JsonLD {
  val httpClient: HttpClient = HttpClientImpl

  def apply(graph: Graph): JsonLD = new JsonLD(graph)
}
class JsonLD(graph: Graph) {
  import JsonLD._

  implicit val ec = monix.execution.Scheduler.global
//  val jsonld      = json.JsonLD(graph)

  trait encode {
    def apply[T <: Node](node: Node): Json =
      Json.jObject(fromNode(node)(ActiveContext()).withContext)

    implicit class WithIriString(iri: String)(implicit activeContext: ActiveContext) {
      lazy val compact: String = activeContext.compactIri(iri)
    }

    def fromNode(node: Node)(implicit activeContext: ActiveContext): JsonObjectInProgress = {
      node match {
        case node: Node if node.labels.contains(DataType.ontology) =>
          fromDataType(DataType.apply(node))
        case node: Node if node.labels.contains(Property.ontology) =>
          fromProperty(Property.apply(node))
        case node: Node if node.labels.contains(Ontology.ontology) =>
          fromOntology(Ontology.apply(node))
        case nodeResource =>
          nodeResource.labels.foldLeft(List[Json]() -> activeContext) {
            case ((iris, activeContext), tpe) =>
              val (iri, newBuilder) = activeContext.compactIri(tpe)
              (iris :+ Json.jString(iri)) -> newBuilder
          } match {
            case (typeIris, activeContext) =>
              val iri = nodeResource.iri
//                if (nodeResource.iri.nonEmpty) nodeResource.iri
//                else nodeResource.graph.iri + "/" + nodeResource.id
              JsonObjectInProgress(
                JsonObject.fromTraversableOnce(Seq(iri).filter(_.nonEmpty).map(types.`@id` -> Json.jString(_)) ++ {
                  val iris = nodeResource.iris
                  if (iris.toList.lengthCompare(1) == 0) Seq(types.`@ids` -> Json.jString(iris.head))
                  else if (iris.nonEmpty)
                    Seq(types.`@ids` -> nodeResource.iris.toList.map(Json.jString).asJson)
                  else Seq()
                } ++ {
                  if (typeIris.lengthCompare(1) == 0) Seq(types.`@type` -> typeIris.head)
                  else if (typeIris.nonEmpty) Seq(types.`@type` -> typeIris.asJson)
                  else Seq()
                }),
                activeContext
              ).addEdges(nodeResource)
          }
      }
    }

    implicit class WithDictionary(jsonIP: JsonObjectInProgress) {
      implicit val activeContext = jsonIP.activeContext
      def addEdges(resource: Resource[_]): JsonObjectInProgress = {
        resource
          .outEMap()
          .filterNot { case (key, properties) => Graph.baseKeys.contains(key) }
          .foldLeft((Map[String, Json](), activeContext)) {
            case ((result, activeContext), (key, edges: List[Edge[_, _]])) if edges.nonEmpty =>
              val (compactIri, newActiveContext)          = activeContext.compactIri(key)
              val JsonInProgress(json, newActiveContext2) = fromEdges(key, edges)(newActiveContext)
              result + (compactIri -> json) -> newActiveContext2
            //TODO: add local @context if parent context already has other overrides
            case ((result, activeContext), (key, edges: List[Edge[_, _]])) => result -> activeContext
          } match {
          case (result, activeContext) =>
            JsonObjectInProgress(JsonObject.fromTraversableOnce(jsonIP.json.toList ++ result), activeContext)
        }
      }
    }

    def fromEdges(key: Property, edges: List[Edge[_, _]])(implicit activeContext: ActiveContext): JsonInProgress = {

      val labelO: Option[ClassType[_]] =
        edges.groupBy(p => p.to.labels.headOption).toList.maxBy(_._2.size)._1

      val newActiveContext =
        if (edges.lengthCompare(4) == 1 && labelO.isDefined &&
            activeContext.properties.get(key).exists(_.`@type`.isEmpty) && {
              if (labelO.exists(l => key.range.headOption.contains(l))) false
              else edges.size / edges.size.toDouble > 0.8
            }) {
          activeContext.copy(
            properties = activeContext.properties + activeContext.properties
              .get(key)
              .map(ap => key -> ap)
              .getOrElse(key -> ActiveProperty(`@type` = labelO.get)))
        } else activeContext

      edges match {
        case null | List() => JsonInProgress(Json.jNull)
        case List(property) /*if key.container.isEmpty*/ =>
          edgeInVToJson(property)(activeContext)
        case edges =>
          edges.foldLeft(List[Json]() -> activeContext) {
            case ((result, activeContext), edge) =>
              edgeInVToJson(edge)(activeContext) match {
                case JsonInProgress(json, activeContext) => (result :+ json) -> activeContext
              }
          } match {
            case (result, activeContext) => JsonInProgress(result.asJson, activeContext)
          }
      }
    }

    private def edgeInVToJson[T](edge: Edge[_, T])(implicit activeContext: ActiveContext): JsonInProgress = {
      fromAny(edge.inV,
              activeContext.properties
                .get(edge.key)
                .flatMap(_.`@type`.headOption)
                .orElse(edge.key.range.headOption))
    }

    private def edgeOutVToJson[T](edge: Edge[_, T])(implicit activeContext: ActiveContext): JsonInProgress = {
      fromAny(edge.outV, None)(activeContext)
    }

    def fromEdge(edge: Edge[_, _])(implicit activeContext: ActiveContext): JsonObjectInProgress = {
      val (keyIri, newActiveContext) = activeContext.compactIri(edge.key)
      JsonObjectInProgress(
        JsonObject.fromTraversableOnce(Seq(edge.iri).filter(_.nonEmpty).map(types.`@id` -> Json.jString(_)) ++ {
          val iris = edge.iris
          if (iris.toList.lengthCompare(1) == 0) Seq(types.`@ids` -> Json.jString(iris.head))
          else if (iris.nonEmpty)
            Seq(types.`@ids` -> edge.iris.toList.map(Json.jString).asJson)
          else Seq()
        } ++ Seq(types.`@type` -> Json.jString(keyIri))),
        activeContext
      ).addEdges(edge)
    }

    def fromData(value: Any, expectedType: DataType[_])(implicit activeContext: ActiveContext): JsonInProgress = {
      expectedType match {
        case label: LiteralType[_]    => fromLiteral(value, label)
        case label: StructuredType[_] => fromStructured(value, label)
      }
    }

    def fromLiteral(value: Any, expectedType: LiteralType[_])(implicit activeContext: ActiveContext): JsonInProgress = {
      expectedType match {
        case label: BoolType[_] =>
          value match {
            case v: Boolean => JsonInProgress(Json.jBool(v))
            case _          => throw ToJsonException(s"boolean expected ${value.getClass} found")
          }
        case label: CalendarType[_] => fromCalendar(value, label)
        case label: NumericType[_]  => fromNumeric(value, label)
        case label: TextType[_]     => fromText(value, label)
      }
    }

    def fromCalendar(value: Any, expectedType: CalendarType[_])(
        implicit activeContext: ActiveContext): JsonInProgress = {
      expectedType match {
        case label: DateTimeType[_]  => fromDateTime(value, label)
        case label: LocalDateType[_] => fromDate(value, label)
        case label: LocalTimeType[_] => fromTime(value, label)
      }
    }

    def fromDateTime(value: Any, expectedType: DateTimeType[_])(
        implicit activeContext: ActiveContext): JsonInProgress = {
      value match {
        case v: Instant => JsonInProgress(Json.jString(v.toString()))
        case _          => throw ToJsonException(s"datetime expected ${value.getClass} found")
      }
    }

    def fromDate(value: Any, expectedType: LocalDateType[_])(implicit activeContext: ActiveContext): JsonInProgress = {
      value match {
        case v: LocalDate => JsonInProgress(Json.jString(v.toString()))
        case _            => throw ToJsonException(s"date expected ${value.getClass} found")
      }
    }

    def fromTime(value: Any, expectedType: LocalTimeType[_])(implicit activeContext: ActiveContext): JsonInProgress = {
      value match {
        case v: LocalTime => JsonInProgress(Json.jString(v.toString()))
        case _ =>
          println(value.asInstanceOf[Resource[_]].value)
          throw ToJsonException(s"time expected ${value.getClass} found")
      }
    }

    def fromNumeric(value: Any, expectedType: NumericType[_])(implicit activeContext: ActiveContext): JsonInProgress = {
      expectedType match {
        case label: IntType[_]    => fromInt(value, label)
        case label: DoubleType[_] => fromDouble(value, label)
        case label: LongType[_]   => fromLong(value, label)
      }
    }

    def fromInt(value: Any, expectedType: IntType[_])(implicit activeContext: ActiveContext): JsonInProgress = {
      value match {
        case v: Int => JsonInProgress(Json.jNumber(v))
        case _ =>
          println(value.asInstanceOf[Resource[_]].value)
          throw ToJsonException(s"int expected ${value.getClass} found")
      }
    }

    def fromDouble(value: Any, expectedType: DoubleType[_])(implicit activeContext: ActiveContext): JsonInProgress = {
      value match {
        case v: Double => JsonInProgress(Json.jNumber(v).get)
        case _         => throw ToJsonException(s"double expected ${value.getClass} found")
      }
    }

    def fromLong(value: Any, expectedType: LongType[_])(implicit activeContext: ActiveContext): JsonInProgress = {
      value match {
        case v: Long => JsonInProgress(Json.jString(v.toString))
        case _       => throw ToJsonException(s"long expected ${value.getClass} found")
      }
    }

    def fromText(value: Any, expectedType: TextType[_])(implicit activeContext: ActiveContext): JsonInProgress = {
      value match {
        case v: String => JsonInProgress(Json.jString(v))
        case _         => throw ToJsonException(s"string expected ${value.getClass} found")
      }
    }

    def fromStructured(value: Any, expectedType: StructuredType[_])(
        implicit activeContext: ActiveContext): JsonInProgress = {
      expectedType match {
        case label: CollectionType[_] => fromCollection(value, label)
//        case label: ColorType[_] =>
        case label: GeometricType[_] => fromGeometric(value, label)
        case label: QuantityType[_]  => fromQuantity(value, label)
        case label: TupleType[_]     => fromTuple(value, label)
      }
    }

    private def toArray(v: Seq[_], label: Option[ClassType[_]])(
        implicit activeContext: ActiveContext): JsonInProgress = {
      val (jsons, ac) = v.foldLeft((List[Json](), activeContext)) {
        case ((r, activeContext), v) =>
          val jip = fromAny(v, label)(activeContext)
          (r :+ jip.json) -> jip.activeContext
      }
      JsonInProgress(jsons.asJson, ac)
    }
    def fromCollection(value: Any, expectedType: CollectionType[_])(
        implicit activeContext: ActiveContext): JsonInProgress = {
      expectedType match {
        case label: ListType[_] =>
          value match {
            case v: List[_] => toArray(v, label.valueRange.headOption)
            case _          => throw ToJsonException(s"list expected ${value.getClass} found")
          }
        case label: MapType[_, _] =>
          value match {
            case v: Map[_, _] =>
              val (tuples, ac) = v.foldLeft((List[(Json, Json)](), activeContext)) {
                case ((r, activeContext), (k, v)) =>
                  val jip  = fromAny(k, label.keyRange.headOption)(activeContext)
                  val jip2 = fromAny(v, label.valueRange.headOption)(jip.activeContext)
                  (r :+ (jip.json, jip2.json)) -> jip2.activeContext
              }
              JsonInProgress(tuples.asJson, ac)
            case _ => throw ToJsonException(s"map expected ${value.getClass} found")
          }
        case label: SetType[_] =>
          value match {
            case v: Set[_] => toArray(v.toSeq, label.valueRange.headOption)
            case _         => throw ToJsonException(s"list expected ${value.getClass} found")
          }
        case label: ListSetType[_] =>
          value match {
            case v: ListSet[_] => toArray(v.toSeq, label.valueRange.headOption)
            case _             => throw ToJsonException(s"list expected ${value.getClass} found")
          }
        case label: VectorType[_] =>
          value match {
            case v: Vector[_] => toArray(v, label.valueRange.headOption)
            case _            => throw ToJsonException(s"list expected ${value.getClass} found")
          }
      }
    }
    def fromGeometric(value: Any, expectedType: GeometricType[_])(
        implicit activeContext: ActiveContext): JsonInProgress = {
      import lspace.encode._
      value match {
        case v: Geometry => JsonInProgress(v.asJson)
        case _           => throw ToJsonException(s"int expected ${value.getClass} found")
      }
    }
    def fromQuantity(value: Any, expectedType: QuantityType[_])(
        implicit activeContext: ActiveContext): JsonInProgress = {
      expectedType match {
        case label: DurationType =>
          value match {
            case v: Time =>
              JsonInProgress(
                Json.jObject(JsonObject.fromTraversableOnce(
                  Map("value" -> Json.jNumber(v.value), "unit" -> Json.jString(v.unit.symbol)))))
            case _ => throw ToJsonException(s"duration expected ${value.getClass} found")
          }
      }
    }
    def fromTuple(value: Any, expectedType: TupleType[_])(implicit activeContext: ActiveContext): JsonInProgress = {
      expectedType match {
        case label: Tuple2Type[_, _] =>
          value match {
            case (v1, v2) =>
              val jip  = fromAny(v1, label._1stRange.headOption)
              val jip2 = fromAny(v2, label._2ndRange.headOption)(jip.activeContext)
              JsonInProgress((jip.json, jip2.json).asJson, jip2.activeContext)
            case _ => throw ToJsonException(s"tuple2 expected ${value.getClass} found")
          }
        case label: Tuple3Type[_, _, _] =>
          value match {
            case (v1, v2, v3) =>
              val jip  = fromAny(v1, label._1stRange.headOption)
              val jip2 = fromAny(v2, label._2ndRange.headOption)(jip.activeContext)
              val jip3 = fromAny(v3, label._3rdRange.headOption)(jip2.activeContext)
              JsonInProgress((jip.json, jip2.json, jip3.json).asJson, jip3.activeContext)
            case _ => throw ToJsonException(s"tuple3 expected ${value.getClass} found")
          }
        case label: Tuple4Type[_, _, _, _] =>
          value match {
            case (v1, v2, v3, v4) =>
              val jip  = fromAny(v1, label._1stRange.headOption)
              val jip2 = fromAny(v2, label._2ndRange.headOption)(jip.activeContext)
              val jip3 = fromAny(v3, label._3rdRange.headOption)(jip2.activeContext)
              val jip4 = fromAny(v4, label._4rdRange.headOption)(jip3.activeContext)
              JsonInProgress((jip.json, jip2.json, jip3.json, jip4.json).asJson, jip4.activeContext)
            case _ => throw ToJsonException(s"tuple4 expected ${value.getClass} found")
          }
      }
    }

    def fromAny(value: Any, expectedType: Option[ClassType[_]] = None)(
        implicit activeContext: ActiveContext): JsonInProgress = {
      value match {
        case resource: IriResource =>
          resource match {
            case value: Value[_] =>
              if (expectedType.contains(value.label)) {
                fromData(value.value, value.label)
              } else {
                val jip = fromData(value.value, value.label)
                JsonInProgress(
                  Json.jObject(
                    JsonObject
                      .fromTraversableOnce(
                        Map(types.`@value` -> jip.json, types.`@type` -> value.label.iri.compact.asJson))),
                  jip.activeContext
                )
              }
            case node: Node =>
              if (node.iri.nonEmpty) JsonInProgress(node.iri.asJson, activeContext)
              else
                fromNode(node).map { (json, activeContext) =>
                  JsonInProgress(Json.jObject(json), activeContext)
                }
            case edge: Edge[_, _] =>
              if (edge.iri.nonEmpty) JsonInProgress(edge.iri.asJson, activeContext)
              else
                fromEdge(edge).map { (json, activeContext) =>
                  JsonInProgress(Json.jObject(json), activeContext)
                }
            case iriResource: IriResource => JsonInProgress(iriResource.iri.asJson, activeContext)
          }
        case _ =>
          val label = ClassType.valueToOntologyResource(value)
          if (expectedType.contains(label)) {
            fromData(value, label)
          } else {
            val jip = fromData(value, label)
            JsonInProgress(
              Json.jObject(
                JsonObject
                  .fromTraversableOnce(Map(types.`@value` -> jip.json, types.`@type` -> label.iri.compact.asJson))),
              jip.activeContext)
          }
      }
    }

    /**
      * ontology to json, TODO: add and return @context
      * @param ontology
      * @return
      */
    def fromOntology(ontology: Ontology)(implicit activeContext: ActiveContext): JsonObjectInProgress = {
      val jsProperties = Seq(
        Some(types.`@id`   -> Json.jString(ontology.iri.compact)),
        Some(types.`@type` -> Json.jString(types.`@class`)),
        ontology.base.map(uri => types.`@base` -> Json.jString(uri.compact)),
        if (ontology.label.nonEmpty)
          Some(types.`@label` -> ontology.label.mapValues(Json.jString).asJson)
        else None,
        if (ontology.comment.nonEmpty)
          Some(types.`@comment` -> ontology.comment.mapValues(Json.jString).asJson)
        else None,
        if (ontology.extendedClasses.nonEmpty)
          Some(types.`@extends` -> ontology.extendedClasses.map(o => Json.jString(o.iri.compact)).asJson)
        else None
      ).flatten.toMap ++
        (ontology.properties.toList match {
          case List()         => Map[String, Json]()
          case List(property) => Map(types.`@properties` -> Json.jString(property.iri.compact))
          case properties =>
            Map(types.`@properties` -> properties.map(key => Json.jString(key.iri.compact)).asJson)
        })

      JsonObjectInProgress(JsonObject.fromTraversableOnce(jsProperties), activeContext)
    }

    /**
      * property to json, TODO: add and return @context
      * @param key
      * @return
      */
    def fromProperty(key: Property)(implicit activeContext: ActiveContext): JsonObjectInProgress = {

      val jsProperties = Seq(
        Some(types.`@id`   -> Json.jString(key.iri.compact)),
        Some(types.`@type` -> Json.jString(types.`@property`)),
        if (key.label.nonEmpty) Some(types.`@label` -> key.label.mapValues(Json.jString).asJson)
        else None,
        if (key.comment.nonEmpty) Some(types.`@comment` -> key.comment.mapValues(Json.jString).asJson)
        else None,
        if (key.container.isDefined)
          Some(types.`@container` -> List(Json.jString(key.container.get)).asJson)
        else None,
        if (key.extendedClasses.nonEmpty)
          Some(types.`@extends` -> key.extendedClasses.map(o => Json.jString(o.iri.compact)).asJson)
        else None
      ).flatten.toMap ++
        (key.range.toList match {
          case List()         => Map[String, Json]()
          case List(dataType) => Map(types.`@range` -> Json.jString(dataType.iri.compact))
          case dataTypes =>
            Map(types.`@range` -> dataTypes.map(dataType => Json.jString(dataType.iri.compact)).asJson)
        }) ++
        (key.properties.toList match {
          case List()         => Map[String, Json]()
          case List(property) => Map(types.`@property` -> Json.jString(property.iri.compact))
          case properties =>
            Map(types.`@property` -> properties.map(key => Json.jString(key.iri.compact)).asJson)
        })

      JsonObjectInProgress(JsonObject.fromTraversableOnce(jsProperties), activeContext)
    }

    def fromDataType(dataType: DataType[_])(implicit activeContext: ActiveContext): JsonObjectInProgress = {
      val jsProperties = Seq(
        Some(types.`@id`   -> Json.jString(dataType.iri)),
        Some(types.`@type` -> Json.jString(types.`@datatype`)),
        if (dataType.label.nonEmpty)
          Some(types.`@label` -> dataType.label.mapValues(Json.jString).asJson)
        else None,
        if (dataType.comment.nonEmpty)
          Some(types.`@comment` -> dataType.comment.mapValues(Json.jString).asJson)
        else None,
        if (dataType.extendedClasses.nonEmpty)
          Some(types.`@extends` -> dataType.extendedClasses.map(o => Json.jString(o.iri)).asJson)
        else None
      ).flatten.toMap ++
        (dataType.properties.toList match {
          case List() => Map[String, Json]()
          //        case List(property) => Map(types.properties -> Json.jString(property.iri))
          case properties =>
            Map(types.`@properties` -> properties.map(key => Json.jString(key.iri)).asJson)
        })

      val (oProperty, newActiveContext) = dataType match {
        case dt: CollectionType[_] =>
          dt match {
            case dt: ListType[_] =>
              val JsonInProgress(l, newActiveContext) = ctListToJson(dt.valueRange)
              if (dt.valueRange.isEmpty || l.string.contains("")) Map() -> newActiveContext
              else Map(CollectionType.keys.valueRange.iri -> l) -> newActiveContext
            case dt: ListSetType[_] =>
              val JsonInProgress(l, newActiveContext) = ctListToJson(dt.valueRange)
              if (dt.valueRange.isEmpty || l.string.contains("")) Map() -> newActiveContext
              else Map(CollectionType.keys.valueRange.iri -> l) -> newActiveContext
            case dt: SetType[_] =>
              val JsonInProgress(l, newActiveContext) = ctListToJson(dt.valueRange)
              if (dt.valueRange.isEmpty || l.string.contains("")) Map() -> newActiveContext
              else Map(CollectionType.keys.valueRange.iri -> l) -> newActiveContext
            case dt: VectorType[_] =>
              val JsonInProgress(l, newActiveContext) = ctListToJson(dt.valueRange)
              if (dt.valueRange.isEmpty || l.string.contains("")) Map() -> newActiveContext
              else Map(CollectionType.keys.valueRange.iri -> l) -> newActiveContext
            case dt: MapType[_, _] =>
              val JsonInProgress(l, newActiveContext)   = ctListToJson(dt.keyRange)
              val JsonInProgress(l2, newActiveContext2) = ctListToJson(dt.valueRange)(newActiveContext)
              if ((dt.keyRange.isEmpty || l.string.exists(_.isEmpty)) && (dt.valueRange.isEmpty || l2.string
                    .exists(_.isEmpty))) Map() -> newActiveContext2
              else if (dt.keyRange.isEmpty || l.string.exists(_.isEmpty))
                Map(CollectionType.keys.valueRange.iri -> l2) -> newActiveContext2
              else if (dt.valueRange.isEmpty || l2.string.exists(_.isEmpty))
                Map(MapType.keys.keyRange.iri -> l) -> newActiveContext2
              else
                Map(MapType.keys.keyRange.iri -> l, CollectionType.keys.valueRange.iri -> l2) -> newActiveContext2
          }
        case _ => Map() -> activeContext
      }
      JsonObjectInProgress(JsonObject.fromTraversableOnce(jsProperties ++ oProperty), newActiveContext)
    }

    def ctListToJson(l: List[ClassType[_]])(implicit activeContext: ActiveContext): JsonInProgress = {
      if (l.lengthCompare(1) == 0 && l.head.iri.isEmpty) JsonInProgress("".asJson)
      else
        l.foldLeft[(List[Json], ActiveContext)](List() -> activeContext) {
          case ((l, activeContext), c) =>
            c match {
              case o: Ontology =>
                activeContext.compactIri(o) match { case (key, activeContext) => (l :+ key.asJson) -> activeContext }
              case p: Property =>
                activeContext.compactIri(p) match { case (key, activeContext) => (l :+ key.asJson) -> activeContext }
              case d: DataType[_] =>
                d match {
                  case d: CollectionType[_] =>
                    fromDataType(d)(activeContext) match {
                      case JsonObjectInProgress(key, activeContext) =>
                        (l :+ Json.jObject(key)) -> activeContext
                    }
                  case _ =>
                    activeContext.compactIri(d) match {
                      case (key, activeContext) => (l :+ key.asJson) -> activeContext
                    }
                }
            }
        } match { case (l, activeContext) => JsonInProgress(l.asJson, activeContext) }
    }
  }
  lazy val encode = new encode {}
  trait decode {
    def toLabeledNode(json: Json, ontology: Ontology, activeContext: ActiveContext = ActiveContext()): Task[Node] = {
      json.obj
        .map { obj =>
          extractContext(obj)(activeContext).flatMap { implicit activeContext =>
            val expandedJson = activeContext.expandKeys(obj)
            extractOntologies(expandedJson)(activeContext).flatMap { ontologies =>
              if (ontologies.contains(ontology)) toNode(expandedJson, None)(activeContext)
              else Task.raiseError(NotAcceptableException(s"cannot parse root object, expected @type ${ontology.iri}"))
            }
          }
        }
        .getOrElse(Task.raiseError(FromJsonException("root must be an object")))
    }

    def toNode(json: Json, activeContext: ActiveContext = ActiveContext()): Task[Node] = {
      json.obj
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
    def toResource(obj: JsonObject, expectedType: Option[ClassType[_]])(
        implicit activeContext: ActiveContext): Task[Resource[Any]] =
      extractContext(obj).flatMap { implicit activeContext =>
        val expandedJson = activeContext.expandKeys(obj)
        extractType(expandedJson).flatMap { expectedTypes =>
          val et = if (expectedTypes.nonEmpty) expectedTypes else expectedType.toList
          tryValue(expandedJson, et.collectFirst { case datatype: DataType[_] => datatype })
            .orElse(toEdge(expandedJson, et.collectFirst { case property: Property => property }))
            .getOrElse {
              et match {
                case (geo: GeometricType[_]) :: tail =>
                  toGeometric(Json.jObject(obj), geo).map { v =>
                    graph.values.create(v, geo)
                  }
                case _ =>
                  toNode(expandedJson, expectedType.collect { case ontology: Ontology => ontology })
              }
            }
        }
      }

    def toResource(json: Json, expectedType: Option[ClassType[_]])(
        implicit activeContext: ActiveContext): Task[Resource[Any]] = {
      json.obj
        .map { obj =>
          toResource(obj, expectedType)
        }
        .getOrElse(toObject(json, expectedType.toList).map {
          case (classtype, resource: Resource[_]) => resource
          case (classtype: DataType[_], value)    => graph.values.create(value, classtype)
        })
    }

    def withEdges[T <: Resource[_]](resource: T, otherJson: Map[String, Json])(
        implicit activeContext: ActiveContext): Task[T] = {
      Task
        .gatherUnordered(otherJson.map {
          case (key, value) =>
            toProperty(key).map(_ -> value)
        })
        .map(_.map {
          case (property, json) =>
            json.obj
              .map(toResource(_, Some(property)).map(resource.addOut(property, _)).map(List(_)))
              .orElse(json.array.map {
                array =>
                  val edgesTask: Task[List[Edge[Any, Any]]] =
                    activeContext.properties.get(property).map(_.`@container`) match {
                      case Some(container)
                          if container.headOption.exists(
                            h => Set[`@container`](`@container`.`@list`, `@container`.`@set`).contains(h)) =>
                        Task.gatherUnordered(array.map {
                          json =>
                            json.obj
                              .map(toResource(_, Some(property)).map(resource.addOut(property, _)))
                              .orElse(
                                json.array
                                  .map { array =>
                                    val expectedType = activeContext.expectedType(property)
                                    expectedType match {
                                      case Some(collectionType: CollectionType[_]) =>
                                        toCollection2(array, collectionType).map(
                                          resource.addOut(property, collectionType, _))
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
                              toCollection2(array, collectionType)
                                .map(resource.addOut(property, collectionType, _))
                                .map(List(_))
                            case _ =>
                              Task.raiseError(
                                FromJsonException("array found without @collection type or @container:@list/@set"))
                          }
                          .getOrElse {
                            Task.raiseError(FromJsonException(
                              "array found without expected @collection type or @container:@list/@set"))
                          }
                    }
                  edgesTask
              })
              .getOrElse {
                val expectedType = activeContext.expectedType(property)
                (for {
                  iri <- json.string
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

    def tryNodeRef(json: Json)(implicit activeContext: ActiveContext): Option[Task[Node]] =
      json.string
        .map(activeContext.expandIri)
        .map(graph.nodes.upsert(_)) //TODO: add label if missing?
        .map(Task.now)
//        .getOrElse(Task.raiseError(FromJsonException("object expected when parsing to node")))

    def toNode(expandedJson: Map[String, Json], label: Option[Ontology])(
        implicit activeContext: ActiveContext): Task[Node] = {
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

    def tryEdgeRef(json: Json, label: Property)(implicit activeContext: ActiveContext): Option[Task[Edge[_, _]]] =
      json.string
        .map(activeContext.expandIri)
        .flatMap(graph.edges.hasIri(_).headOption) //TODO: check if label == edge.key and throw exception if !=
        .map(Task.now)
    def toEdge(expandedJson: Map[String, Json], expectedType: Option[Property])(
        implicit activeContext: ActiveContext): Option[Task[Edge[Any, Any]]] = toEdge(expandedJson, expectedType.toList)
    def toEdge(expandedJson: Map[String, Json], expectedTypes: List[Property])(
        implicit activeContext: ActiveContext): Option[Task[Edge[Any, Any]]] = {
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

    def toLiteral[T](json: Json, label: LiteralType[T])(implicit activeContext: ActiveContext): Task[T] =
      (label match {
        case TextType.datatype =>
          json.string
        case tpe: NumericType[_] =>
          tpe match {
            case DoubleType.datatype => json.number.flatMap(_.toDouble)
            case LongType.datatype   => json.string.map(_.toLong)
            case IntType.datatype    => json.number.flatMap(_.toInt)
          }
        case tpe: CalendarType[_] =>
          tpe match {
            case DateTimeType.datatype  => json.string.map(Instant.parse(_))
            case LocalDateType.datatype => json.string.map(LocalDate.parse(_))
            case LocalTimeType.datatype => json.string.map(LocalTime.parse(_))
          }
        //        case tpe: ColorType[_] =>
        case BoolType.datatype => json.bool
        case _                 => None
      }).map(_.asInstanceOf[T])
        .map(Task.now)
        .getOrElse(Task.raiseError(UnexpectedJsonException(s"unknown LiteralType ${label.iri}")))

    def toStructured[T](json: Json, label: StructuredType[T])(implicit activeContext: ActiveContext): Task[T] =
      label match {
        case label: GeometricType[T] =>
          toGeometric(json, label)
        case label: CollectionType[T] =>
          toCollection(json, label)
        case label: TupleType[T] =>
          toTuple(json, label)
        case _ => Task.raiseError(UnexpectedJsonException(s"unknown StructuredType ${label.iri}"))
      }

    def toGeometric[T](json: Json, label: GeometricType[T])(implicit activeContext: ActiveContext): Task[T] = {
      import lspace.decode._
      (label match { //TODO: create specific parsers
        case label: GeopointType[_]   => fromGeoJson(json).map(_.asInstanceOf[Point])
        case label: GeoPolygonType[_] => fromGeoJson(json).map(_.asInstanceOf[Polygon])
        case _                        => None
      }).map(_.asInstanceOf[T])
        .map(Task.now)
        .getOrElse(Task.raiseError(UnexpectedJsonException(s"unknown GeometricType ${label.iri}")))
    }

    def toCollection2[T](json: JsonArray, label: CollectionType[T])(implicit activeContext: ActiveContext): Task[T] =
      toCollection(Json.jArray(json), label)

    def toCollection[T](json: Json, label: CollectionType[T])(implicit activeContext: ActiveContext): Task[T] =
      label match {
        case label: ListType[_]    => toList(json, label.valueRange).map(_.asInstanceOf[T])
        case label: VectorType[_]  => toVector(json, label.valueRange).map(_.asInstanceOf[T])
        case label: SetType[_]     => toSet(json, label.valueRange).map(_.asInstanceOf[T])
        case label: ListSetType[_] => toListSet(json, label.valueRange).map(_.asInstanceOf[T])
        case label: MapType[_, _]  => toMap(json, label.keyRange, label.valueRange).map(_.asInstanceOf[T])
        case _                     => Task.raiseError(UnexpectedJsonException(s"unknown CollectionType ${label.iri}"))
      }

    def toTuple[T](json: Json, label: TupleType[T])(implicit activeContext: ActiveContext): Task[T] =
      label match {
        case dt: Tuple2Type[_, _] => toTuple2(json, dt._1stRange, dt._2ndRange).map(label -> _).map(_.asInstanceOf[T])
        case dt: Tuple3Type[_, _, _] =>
          toTuple3(json, dt._1stRange, dt._2ndRange, dt._3rdRange).map(label -> _).map(_.asInstanceOf[T])
        case dt: Tuple4Type[_, _, _, _] =>
          toTuple4(json, dt._1stRange, dt._2ndRange, dt._3rdRange, dt._4rdRange).map(label -> _).map(_.asInstanceOf[T])
        case _ => Task.raiseError(UnexpectedJsonException(s"unknown TupleType ${label.iri}"))
      }

    def toObject(json: Json, label: List[ClassType[_]])(
        implicit activeContext: ActiveContext): Task[(ClassType[Any], Any)] = {
      json.obj
        .map { obj =>
          extractContext(obj).flatMap { implicit activeContext =>
            val expandedJson = activeContext.expandKeys(obj)
            activeContext
              .extractValue(expandedJson) { json =>
                extractDatatype(expandedJson).map(_.orElse(label.headOption)).flatMap { labelOption =>
                  labelOption
                    .map {
                      case tpe: DataType[_] if label.nonEmpty && !label.contains(tpe) =>
                        Task.raiseError(
                          UnexpectedJsonException("a collection can only have value with the @valueRange"))
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

    def tryRaw(json: Json)(implicit activeContext: ActiveContext): Option[Task[(ClassType[Any], Any)]] = {
      //is url
      //is primitive
      toPrimitive(json).map(v => ClassType.valueToOntologyResource(v) -> v).map(Task.now)
    }

    def toObject(expandedJson: Map[String, Json], expectedType: Option[ClassType[_]])(
        implicit activeContext: ActiveContext): Task[(ClassType[Any], Any)] =
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
        implicit activeContext: ActiveContext): Option[Task[Any]] = {
      activeContext
        .extractValue(expandedJson) { json =>
          extractDatatype(expandedJson).map(_.orElse(expectedType)).flatMap {
            _.map { label =>
              toData(json, label)
            }.getOrElse(Task.raiseError(UnexpectedJsonException("cannot parse value without type")))
          }
        }
    }

    def toData(json: Json, label: DataType[_])(implicit activeContext: ActiveContext): Task[Any] =
      label match {
        case label: LiteralType[Any] =>
          toLiteral(json, label)
        case label: StructuredType[Any] =>
          toStructured(json, label)
        case _ => Task.raiseError(UnexpectedJsonException(s"unknown DataType ${label.iri}"))
      }

    def tryValue(expandedJson: Map[String, Json], expectedType: Option[DataType[_]])(
        implicit activeContext: ActiveContext): Option[Task[Value[Any]]] = {
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

    def toValue(json: Json, label: DataType[_])(implicit activeContext: ActiveContext): Task[Value[Any]] =
      toData(json, label).map { v =>
        graph.values.create(v, label)
      }

    //Int, Long, Double or String
    def toPrimitive(json: Json): Option[Any] =
      json.number
        .flatMap(n => n.toInt.orElse(n.toLong).orElse(n.toDouble))
        .orElse(json.string)

//    def toResource(json: Json, label: Option[ClassType[_]] = None)(
//        implicit builder: ActiveContext): Task[Resource[Any]] = {
//      label
//        .map {
//          case ontology: Ontology    => toNode(json, Some(ontology))
//          case property: Property    => toEdge(json, property)
//          case datatype: DataType[_] => toValue(json, datatype)
//          case _                     => Task.raiseError(FromJsonException("label is not an ontology, property or datatype???"))
//        }
//        .getOrElse {
//          json.obj
//            .map { obj =>
//              val expandedJson = builder.expandKeys(obj)
//              val types        = extractType(expandedJson)
//              types.flatMap {
//                case List(label: DataType[Any])                                        => toValue(json, label)
//                case List(label: Property)                                             => toEdge(json, label)
//                case labels: List[Ontology] if labels.forall(_.isInstanceOf[Ontology]) => toNode(json, labels)
//                case _                                                                 => Task.raiseError(FromJsonException("cannot decode object"))
//              }
//            }
//            .orElse(json.string.map(graph.nodes.upsert(_)).map(Task.now))
//            .getOrElse(Task.raiseError(FromJsonException("cannot decode resource")))
//        }
//    }

    /**
      * gets list or iris
      * @param json
      * @param activeContext
      * @return
      */
    def extractRefs(json: Json)(implicit activeContext: ActiveContext): List[String] =
      json.array
        .map(array =>
          array.flatMap(json =>
            json.obj
              .flatMap { obj =>
                val expandedJson = activeContext.expandKeys(obj)
                activeContext.extractId(expandedJson)
              }
              .orElse(json.string)))
        .orElse(json.string.map(List(_)))
        .getOrElse(List())
        .map(activeContext.expandIri)

    def toOntology(obj: JsonObject)(implicit activeContext: ActiveContext): Task[Ontology] = {
      val expandedJson = activeContext.expandKeys(obj)
      if (expandedJson.get(types.`@type`).exists(_.string.exists(_ == types.`@class`))) {
        activeContext.extractId(expandedJson) match {
          case Some(iri) =>
            graph.ns.ontologies.get(iri).map(Task.now).getOrElse {
              Ontology.getOrConstructing(iri)(Task.evalOnce {
                val iris          = activeContext.extractIds(expandedJson)
                val labels        = activeContext.extractLabels(expandedJson)
                val comments      = activeContext.extractComments(expandedJson)
                val `@extends`    = expandedJson.get(types.`@extends`).orElse(expandedJson.get(types.rdfsSubClassOf))
                val `@properties` = expandedJson.get(types.`@properties`)

                Task
                  .gatherUnordered(`@extends`.map(toOntologies).getOrElse(Task.now(List())) :: `@properties`.map(
                    toProperties).getOrElse(Task.now(List())) :: Nil)
                  .map {
                    l =>
                      Ontology._Ontology(iri)(
                        iris.toSet,
                        label = labels,
                        comment = comments,
                        _extendedClasses = () =>
                          `@extends`.map(extractRefs)
                            .getOrElse(List())
                            .map(Ontology.getConstructed)
                            .map(_.getOrElse(throw FromJsonException(s"constructed ontology not found")).value),
                        _properties = () =>
                          `@properties`.map(extractRefs)
                            .getOrElse(List())
                            .map(Property.getConstructed)
                            .map(_.getOrElse(throw FromJsonException(s"constructed ontology not found")).value)
                      )
                  }
              }.flatten)
            }
          case None =>
            Task.raiseError(FromJsonException("an ontology without @id is not valid"))
        }
      } else Task.raiseError(FromJsonException("ontology is not of type '@class'"))
    }
    def toOntologies(json: Json)(implicit activeContext: ActiveContext): Task[List[Ontology]] =
      json.array
        .map(
          _.map(
            json =>
              json.obj
                .map(toOntology)
                .orElse(json.string
                  .map(activeContext.expandIri)
                  .map(iri => graph.ns.ontologies.get(iri).map(Task.now).getOrElse(fetchOntology(iri))))
                .getOrElse(Task.raiseError(FromJsonException("nested arrays not allowed")))))
        .map(Task.gather(_))
        .orElse(json.string
          .map(activeContext.expandIri)
          .map(iri =>
            graph.ns.ontologies.get(iri).map(Task.now(_).map(List(_))).getOrElse(fetchOntology(iri).map(List(_)))))
        .getOrElse(Task.raiseError(FromJsonException("ontology should be a string or object")))

    def toProperty(obj: JsonObject)(implicit activeContext: ActiveContext): Task[Property] = {
      val expandedJson = activeContext.expandKeys(obj)
      if (expandedJson.get(types.`@type`).exists(_.string.exists(_ == types.`@property`))) {
        activeContext.extractId(expandedJson) match {
          case Some(iri) =>
            graph.ns.properties.get(iri).map(Task.now).getOrElse {
              Property.getOrConstructing(iri)(Task.evalOnce {
                val iris          = activeContext.extractIds(expandedJson)
                val labels        = activeContext.extractLabels(expandedJson)
                val comments      = activeContext.extractComments(expandedJson)
                val `@range`      = expandedJson.get(types.`@range`).orElse(expandedJson.get(types.schemaRange))
                val `@extends`    = expandedJson.get(types.`@extends`).orElse(expandedJson.get(types.rdfsSubPropertyOf))
                val `@properties` = expandedJson.get(types.`@properties`)

                Task
                  .gatherUnordered(
                    `@range`.map(toOntologies).getOrElse(Task.now(List())) ::
                      `@extends`.map(toOntologies).getOrElse(Task.now(List())) ::
                      `@properties`.map(toProperties).getOrElse(Task.now(List())) :: Nil)
                  .map {
                    l =>
                      Property._Property(iri)(
                        iris.toSet,
                        label = labels,
                        comment = comments,
                        _extendedClasses = () =>
                          `@extends`.map(extractRefs)
                            .getOrElse(List())
                            .map(Property.getConstructed)
                            .map(_.getOrElse(throw FromJsonException(s"constructed property not found")).value),
                        _properties = () =>
                          `@properties`.map(extractRefs)
                            .getOrElse(List())
                            .map(Property.getConstructed)
                            .map(_.getOrElse(throw FromJsonException(s"constructed property not found")).value)
                      )
                  }
              }.flatten)
            }
          case None =>
            Task.raiseError(FromJsonException("a property without @id is not valid"))
        }
      } else Task.raiseError(FromJsonException("property is not of type '@class'"))
    }
    def toProperty(iri: String)(implicit activeContext: ActiveContext): Task[Property] =
      graph.ns.properties.get(iri).map(Task.now).getOrElse(Property.getConstructing(iri).getOrElse(fetchProperty(iri)))

    def toProperties(json: Json)(implicit activeContext: ActiveContext): Task[List[Property]] =
      json.array
        .map(
          _.map(
            json =>
              json.obj
                .map(toProperty)
                .orElse(json.string
                  .map(activeContext.expandIri)
                  .map(iri => graph.ns.properties.get(iri).map(Task.now).getOrElse(fetchProperty(iri))))
                .getOrElse(Task.raiseError(FromJsonException("nested arrays not allowed")))))
        .map(Task.gather(_))
        .orElse(json.string
          .map(activeContext.expandIri)
          .map(iri =>
            graph.ns.properties.get(iri).map(Task.now(_).map(List(_))).getOrElse(fetchProperty(iri).map(List(_)))))
        .getOrElse(Task.raiseError(FromJsonException("property should be a string or object")))

    def toListType(expandedJson: Map[String, Json])(implicit activeContext: ActiveContext): Task[ListType[Any]] = {
      val `@valueRange` = expandedJson.get(ListType.keys.valueRange.iri)
      Task
        .gatherUnordered(`@valueRange`.map(toClasstypes).getOrElse(Task.now(List())) :: Nil)
        .map { l =>
          ListType(l.flatten)
//            `@valueRange`.map(extractRefs)
//              .getOrElse(List())
//              .map(DataType.getConstructed)
//              .map(_.getOrElse(throw FromJsonException(s"constructed datatype not found")).value))
        }
    }

    def toSetType(expandedJson: Map[String, Json])(implicit activeContext: ActiveContext): Task[SetType[Any]] = {
      val `@valueRange` = expandedJson.get(SetType.keys.valueRange.iri)
      Task
        .gatherUnordered(`@valueRange`.map(toClasstypes).getOrElse(Task.now(List())) :: Nil)
        .map { l =>
          SetType(l.flatten)
//            `@valueRange`.map(extractRefs)
//              .getOrElse(List())
//              .map(DataType.getConstructed)
//              .map(_.getOrElse(throw FromJsonException(s"constructed datatype not found")).value))
        }
    }

    def toListSetType(expandedJson: Map[String, Json])(
        implicit activeContext: ActiveContext): Task[ListSetType[Any]] = {
      val `@valueRange` = expandedJson.get(ListSetType.keys.valueRange.iri)
      Task
        .gatherUnordered(`@valueRange`.map(toClasstypes).getOrElse(Task.now(List())) :: Nil)
        .map { l =>
          ListSetType(l.flatten)
//            `@valueRange`.map(extractRefs)
//              .getOrElse(List())
//              .map(DataType.getConstructed)
//              .map(_.getOrElse(throw FromJsonException(s"constructed datatype not found")).value))
        }
    }

    def toVectorType(expandedJson: Map[String, Json])(implicit activeContext: ActiveContext): Task[VectorType[Any]] = {
      val `@valueRange` = expandedJson.get(VectorType.keys.valueRange.iri)
      Task
        .gatherUnordered(`@valueRange`.map(toClasstypes).getOrElse(Task.now(List())) :: Nil)
        .map { l =>
          VectorType(l.flatten)
//            `@valueRange`.map(extractRefs)
//              .getOrElse(List())
//              .map(DataType.getConstructed)
//              .map(_.getOrElse(throw FromJsonException(s"constructed datatype not found")).value))
        }
    }

    def toMapType(expandedJson: Map[String, Json])(implicit activeContext: ActiveContext): Task[MapType[Any, Any]] = {
      val `@keyRange`   = expandedJson.get(MapType.keys.keyRange.iri)
      val `@valueRange` = expandedJson.get(MapType.keys.valueRange.iri)
      Task
        .gatherUnordered(
          `@keyRange`.map(toClasstypes).getOrElse(Task.now(List())) ::
            `@valueRange`.map(toClasstypes).getOrElse(Task.now(List())) :: Nil)
        .map { l =>
          MapType(l(0), l(1))
//            `@keyRange`.map(extractRefs)
//              .getOrElse(List())
//              .map(
//                iri =>
//                  Ontology
//                    .getConstructed(iri)
//                    .orElse(Property.getConstructed(iri))
//                    .orElse(DataType.getConstructed(iri)))
//              .map(_.getOrElse(throw FromJsonException(s"constructed datatype not found")).value),
//            `@valueRange`.map(extractRefs)
//              .getOrElse(List())
//              .map(
//                iri =>
//                  Ontology
//                    .getConstructed(iri)
//                    .orElse(Property.getConstructed(iri))
//                    .orElse(DataType.getConstructed(iri)))
//              .map(_.getOrElse(throw FromJsonException(s"constructed datatype not found")).value)
//          )
        }
    }

    def toTuple2Type(expandedJson: Map[String, Json])(
        implicit activeContext: ActiveContext): Task[Tuple2Type[Any, Any]] = {
      val _1stRange = expandedJson.get(TupleType.keys._1stRange.iri)
      val _2ndRange = expandedJson.get(TupleType.keys._2ndRange.iri)
      Task
        .gatherUnordered(
          _1stRange.map(toClasstypes).getOrElse(Task.now(List())) ::
            _2ndRange.map(toClasstypes).getOrElse(Task.now(List())) :: Nil)
        .map { l =>
          Tuple2Type(l(0), l(1))
//            _1stRange
//              .map(extractRefs)
//              .getOrElse(List())
//              .map(
//                iri =>
//                  Ontology
//                    .getConstructed(iri)
//                    .orElse(Property.getConstructed(iri))
//                    .orElse(DataType.getConstructed(iri)))
//              .map(_.getOrElse(throw FromJsonException(s"constructed datatype not found")).value),
//            _2ndRange
//              .map(extractRefs)
//              .getOrElse(List())
//              .map(
//                iri =>
//                  Ontology
//                    .getConstructed(iri)
//                    .orElse(Property.getConstructed(iri))
//                    .orElse(DataType.getConstructed(iri)))
//              .map(_.getOrElse(throw FromJsonException(s"constructed datatype not found")).value)
//          )
        }
    }

    def toTuple3Type(expandedJson: Map[String, Json])(
        implicit activeContext: ActiveContext): Task[Tuple3Type[Any, Any, Any]] = {
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
//            _1stRange
//              .map(extractRefs)
//              .getOrElse(List())
//              .map(
//                iri =>
//                  Ontology
//                    .getConstructed(iri)
//                    .orElse(Property.getConstructed(iri))
//                    .orElse(DataType.getConstructed(iri)))
//              .map(_.getOrElse(throw FromJsonException(s"constructed datatype not found")).value),
//            _2ndRange
//              .map(extractRefs)
//              .getOrElse(List())
//              .map(
//                iri =>
//                  Ontology
//                    .getConstructed(iri)
//                    .orElse(Property.getConstructed(iri))
//                    .orElse(DataType.getConstructed(iri)))
//              .map(_.getOrElse(throw FromJsonException(s"constructed datatype not found")).value),
//            _3rdRange
//              .map(extractRefs)
//              .getOrElse(List())
//              .map(
//                iri =>
//                  Ontology
//                    .getConstructed(iri)
//                    .orElse(Property.getConstructed(iri))
//                    .orElse(DataType.getConstructed(iri)))
//              .map(_.getOrElse(throw FromJsonException(s"constructed datatype not found")).value)
//          )
        }
    }

    def toTuple4Type(expandedJson: Map[String, Json])(
        implicit activeContext: ActiveContext): Task[Tuple4Type[Any, Any, Any, Any]] = {
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
//            _1stRange
//              .map(extractRefs)
//              .getOrElse(List())
//              .map(
//                iri =>
//                  Ontology
//                    .getConstructed(iri)
//                    .orElse(Property.getConstructed(iri))
//                    .orElse(DataType.getConstructed(iri)))
//              .map(_.getOrElse(throw FromJsonException(s"constructed datatype not found")).value),
//            _2ndRange
//              .map(extractRefs)
//              .getOrElse(List())
//              .map(
//                iri =>
//                  Ontology
//                    .getConstructed(iri)
//                    .orElse(Property.getConstructed(iri))
//                    .orElse(DataType.getConstructed(iri)))
//              .map(_.getOrElse(throw FromJsonException(s"constructed datatype not found")).value),
//            _3rdRange
//              .map(extractRefs)
//              .getOrElse(List())
//              .map(
//                iri =>
//                  Ontology
//                    .getConstructed(iri)
//                    .orElse(Property.getConstructed(iri))
//                    .orElse(DataType.getConstructed(iri)))
//              .map(_.getOrElse(throw FromJsonException(s"constructed datatype not found")).value),
//            _4rdRange
//              .map(extractRefs)
//              .getOrElse(List())
//              .map(
//                iri =>
//                  Ontology
//                    .getConstructed(iri)
//                    .orElse(Property.getConstructed(iri))
//                    .orElse(DataType.getConstructed(iri)))
//              .map(_.getOrElse(throw FromJsonException(s"constructed datatype not found")).value)
//          )
        }
    }

    def toDatatype(obj: JsonObject)(implicit activeContext: ActiveContext): Task[DataType[Any]] = {
      val expandedJson = activeContext.expandKeys(obj)
      if (expandedJson.get(types.`@type`).exists(_.string.exists(_ == types.`@datatype`))) {
        activeContext.extractId(expandedJson) match {
          case Some(iri) =>
            graph.ns.datatypes.get(iri).orElse(collectionIri(iri)).map(Task.now).getOrElse {
              DataType.getOrConstructing(iri)(Task.evalOnce {
                val iris     = activeContext.extractIds(expandedJson)
                val labels   = activeContext.extractLabels(expandedJson)
                val comments = activeContext.extractComments(expandedJson)
                val `@extends` = expandedJson
                  .get(types.`@extends`)
                  .orElse(expandedJson.get(types.rdfsSubClassOf))
                  .map(extractRefs)
                  .getOrElse(List())
                if (`@extends`.lengthCompare(1) == 1)
                  Task.raiseError(FromJsonException("datatype cannot extend multiple datatypes"))
                else if (`@extends`.isEmpty) {
                  if (iri.startsWith("@"))
                    graph.ns.datatypes
                      .get(iri)
                      .orElse(collectionIri(iri))
                      .map(Task.now)
                      .getOrElse(Task.raiseError(FromJsonException("not a collection-iri")))
                  else Task.raiseError(FromJsonException("datatype should start with '@'"))
                } else
                  `@extends`.head match {
                    case types.`@list` =>
                      toListType(expandedJson)
                    case types.`@set` =>
                      toSetType(expandedJson)
                    case types.`@listset` =>
                      toListSetType(expandedJson)
                    case types.`@vector` =>
                      toVectorType(expandedJson)
                    case types.`@map` =>
                      toMapType(expandedJson)
                    case types.`@tuple2` =>
                      toTuple2Type(expandedJson)
                    case types.`@tuple3` =>
                      toTuple3Type(expandedJson)
                    case types.`@tuple4` =>
                      toTuple4Type(expandedJson)
                    case otherType =>
                      graph.ns.datatypes
                        .get(otherType)
                        .map(Task.now)
                        .getOrElse(Task.raiseError(FromJsonException("toDatatype error")))
                  }
              }.flatten)
            }
          case None =>
            Task.raiseError(FromJsonException("an datatype without @id is not valid"))
        }
      } else Task.raiseError(FromJsonException("datatype is not of type '@class'"))
    }

    private def collectionIri(iri: String)(implicit activeContext: ActiveContext): Option[DataType[_]] = {
      def iriToCollectionType(iri: String, tail: String): (StructuredType[Any], String) = iri match {
        case types.`@list` =>
          val (list, newTail) = getIris(tail, 1)
          ListType(list.head) -> newTail
        case types.`@listset` =>
          val (list, newTail) = getIris(tail, 1)
          ListSetType(list.head) -> newTail
        case types.`@set` =>
          val (list, newTail) = getIris(tail, 1)
          ListType(list.head) -> newTail
        case types.`@vector` =>
          val (list, newTail) = getIris(tail, 1)
          ListType(list.head) -> newTail
        case types.`@map` =>
          val (list, newTail) = getIris(tail, 2)
          MapType(list.head, list(1)) -> newTail
        case types.`@tuple2` =>
          val (list, newTail) = getIris(tail, 2)
          Tuple2Type(list.head, list(1)) -> newTail
        case types.`@tuple3` =>
          val (list, newTail) = getIris(tail, 3)
          Tuple3Type(list.head, list(1), list(2)) -> newTail
        case types.`@tuple4` =>
          val (list, newTail) = getIris(tail, 4)
          Tuple4Type(list.head, list(1), list(2), list(3)) -> newTail
        //TODO: catch others
      }
      def getIris(tail: String, parts: Int = 1): (List[List[ClassType[Any]]], String) = {
        val indexClosingParenthesis = tail.indexOf(')')
        val indexOpeningParenthesis = tail.indexOf('(')
        if (indexClosingParenthesis < indexOpeningParenthesis) {
          val (tailClasstypes, tailIri) =
            if (parts > 1) getIris(tail.drop(indexClosingParenthesis + 1), parts - 1)
            else List[List[ClassType[_]]]() -> tail //get other parts
          (tail
            .take(indexClosingParenthesis)
            .split('+')
            .toList
            .map(iriToClassType(_).getOrElse(throw FromJsonException("unknown @type"))) :: tailClasstypes) -> tailIri //concat
        } else {

          tail
            .take(indexOpeningParenthesis)
            .split('+')
            .toList match { //split types until nested type @int+@double+@list/(
            case head :: Nil =>
              val (tpe, newTail) = iriToCollectionType(head.stripSuffix("/"), tail.drop(indexOpeningParenthesis + 1))
              val (tailTpes, newTail2) =
                if (parts > 2) getIris(newTail, parts - 2)
                else List[List[ClassType[_]]]() -> tail
              (List(tpe) :: tailTpes) -> newTail2
            case list =>
              val (tpe, newTail) =
                iriToCollectionType(list.last.stripSuffix("/"), tail.drop(indexOpeningParenthesis + 1))
              val (tailTpes, newTail2) =
                if (parts > 1 + list.size) getIris(newTail, parts - (1 + list.size))
                else List[List[ClassType[_]]]() -> tail
              (List((list
                .dropRight(1)
                .map(iriToClassType(_).getOrElse(throw FromJsonException("unknown @type"))) :+ tpe) ::: tailTpes.head) ::: tailTpes.tail) -> newTail2
          }
        }
      }
      iri.replaceAll("/", "").split("(", 2).toList match {
        case List(iri, tail) =>
          Some(iri match {
            case types.`@list`    => iriToCollectionType(iri, tail)._1
            case types.`@listset` => iriToCollectionType(iri, tail)._1
            case types.`@set`     => iriToCollectionType(iri, tail)._1
            case types.`@vector`  => iriToCollectionType(iri, tail)._1
            case types.`@map`     => iriToCollectionType(iri, tail)._1
            case types.`@tuple2`  => iriToCollectionType(iri, tail)._1
            case types.`@tuple3`  => iriToCollectionType(iri, tail)._1
            case types.`@tuple4`  => iriToCollectionType(iri, tail)._1
          })
      }
    }

    def iriToClassType(iri: String)(implicit activeContext: ActiveContext): Option[ClassType[Any]] =
      graph.ns.ontologies
        .get(iri)
        .orElse(graph.ns.properties.get(iri))
        .orElse(graph.ns.datatypes.get(iri))
        .orElse(collectionIri(iri))

    def toClasstype(obj: JsonObject)(implicit activeContext: ActiveContext): Task[ClassType[Any]] = {
      val expandedJson = activeContext.expandKeys(obj)
      if (expandedJson.get(types.`@type`).exists(_.string.exists(_ == types.`@datatype`))) {
        toDatatype(obj)
      } else if (expandedJson.get(types.`@type`).exists(_.string.exists(_ == types.`@class`))) {
        toOntology(obj)
      } else if (expandedJson.get(types.`@type`).exists(_.string.exists(_ == types.`@property`))) {
        toProperty(obj)
      } else {
        Task.raiseError(FromJsonException("cannot decode classtype"))
      }
    }

    def toClasstypes(json: Json)(implicit activeContext: ActiveContext): Task[List[ClassType[Any]]] = {
      json.array
        .map(
          _.map(
            json =>
              json.obj
                .map(toClasstype)
                .orElse(
                  json.string
                    .map(activeContext.expandIri)
                    .map(
                      iri =>
                        graph.ns.ontologies
                          .get(iri)
                          .orElse(graph.ns.properties.get(iri))
                          .orElse(graph.ns.datatypes.get(iri))
                          .orElse(collectionIri(iri))
                          .map(Task.now)
                          .getOrElse(fetchProperty(iri))))
                .getOrElse(Task.raiseError(FromJsonException("nested arrays not allowed")))))
        .map(Task.gather(_))
        .orElse(json.string
          .map(activeContext.expandIri)
          .map(iri =>
            graph.ns.properties.get(iri).map(Task.now(_).map(List(_))).getOrElse(fetchProperty(iri).map(List(_)))))
        .getOrElse(Task.raiseError(FromJsonException("property should be a string or object")))
    }

    def toList(json: Json, label: List[ClassType[_]])(implicit activeContext: ActiveContext): Task[List[Any]] =
      json.array
        .map(array =>
          Task.gather {
            array.map { json =>
              toObject(json, label).map(_._2)
            }
        })
        .getOrElse(Task.raiseError(UnexpectedJsonException("not a list")))

    def toSet(json: Json, label: List[ClassType[_]])(implicit activeContext: ActiveContext): Task[Set[Any]] =
      json.array
        .map(array =>
          Task.gather {
            array.toSet.map { json: Json =>
              toObject(json, label).map(_._2)
            }
        })
        .getOrElse(Task.raiseError(UnexpectedJsonException("not a list")))

    def toListSet(json: Json, label: List[ClassType[_]])(implicit activeContext: ActiveContext): Task[ListSet[Any]] =
      json.array
        .map(
          array =>
            Task
              .gather {
                array.map { json =>
                  toObject(json, label).map(_._2)
                }
              }
              .map(_.to[ListSet]))
        .getOrElse(Task.raiseError(UnexpectedJsonException("not a list")))

    def toVector(json: Json, label: List[ClassType[_]])(implicit activeContext: ActiveContext): Task[Vector[Any]] =
      json.array
        .map(array =>
          Task.gather {
            array.toVector.map { json =>
              toObject(json, label).map(_._2)
            }
        })
        .getOrElse(Task.raiseError(UnexpectedJsonException("not a list")))

    def toMap(json: Json, keyLabel: List[ClassType[_]], valueLabel: List[ClassType[_]])(
        implicit activeContext: ActiveContext): Task[Map[Any, Any]] =
      json.array
        .map(array =>
          Task.gather {
            array.map { json =>
              json.array
                .map {
                  case List(key, value) =>
                    Task.parMap2(toObject(key, keyLabel).map(_._2), toObject(value, valueLabel).map(_._2))(_ -> _)
                  case _ => Task.raiseError(UnexpectedJsonException("not a map structure"))
                }
                .getOrElse(Task.raiseError(UnexpectedJsonException("not a map structure")))
            }
        })
        .getOrElse(Task.raiseError(UnexpectedJsonException("not a map structure")))
        .map(_.toMap)

    def toTuple2(json: Json, label1: List[ClassType[_]], label2: List[ClassType[_]])(
        implicit activeContext: ActiveContext): Task[(Any, Any)] =
      json.array
        .map {
          case List(v1, v2) =>
            Task.parMap2(toObject(v1, label1).map(_._2), toObject(v2, label2).map(_._2))(_ -> _)
          case _ => Task.raiseError(UnexpectedJsonException("not a tuple2 structure"))
        }
        .getOrElse(Task.raiseError(UnexpectedJsonException("not a tuple2 structure")))

    def toTuple3(json: Json, label1: List[ClassType[_]], label2: List[ClassType[_]], label3: List[ClassType[_]])(
        implicit activeContext: ActiveContext): Task[(Any, Any, Any)] =
      json.array
        .map {
          case List(v1, v2, v3) =>
            Task.parMap3(toObject(v1, label1).map(_._2),
                         toObject(v2, label2).map(_._2),
                         toObject(v3, label3).map(_._2))((_, _, _))
          case _ => Task.raiseError(UnexpectedJsonException("not a tuple3 structure"))
        }
        .getOrElse(Task.raiseError(UnexpectedJsonException("not a tuple3 structure")))

    def toTuple4(json: Json,
                 label1: List[ClassType[_]],
                 label2: List[ClassType[_]],
                 label3: List[ClassType[_]],
                 label4: List[ClassType[_]])(implicit activeContext: ActiveContext): Task[(Any, Any, Any, Any)] =
      json.array
        .map {
          case List(v1, v2, v3, v4) =>
            Task.parMap4(toObject(v1, label1).map(_._2),
                         toObject(v2, label2).map(_._2),
                         toObject(v3, label3).map(_._2),
                         toObject(v4, label4).map(_._2))((_, _, _, _))
          case _ => Task.raiseError(UnexpectedJsonException("not a tuple4 structure"))
        }
        .getOrElse(Task.raiseError(UnexpectedJsonException("not a tuple4 structure")))

    def extractOntologies(obj: Map[String, Json])(implicit activeContext: ActiveContext): Task[List[Ontology]] =
      obj.get(types.`@type`).map(toOntologies(_)).getOrElse(Task.now(List()))
    def extractProperties(obj: Map[String, Json])(implicit activeContext: ActiveContext): Task[Option[Property]] =
      obj.get(types.`@type`).map(toProperties(_)).map(_.map(_.headOption)).getOrElse(Task.now(None))
    def extractDatatype(obj: Map[String, Json])(implicit activeContext: ActiveContext): Task[Option[DataType[Any]]] =
      obj
        .get(types.`@type`)
        .flatMap(
          json =>
            json.obj
              .map(toDatatype(_).map(Option(_)))
              .orElse(json.string.map(graph.ns.datatypes.get(_)).map(Task.now)))
        .getOrElse(Task.now(None))

    def extractType(obj: Map[String, Json])(implicit activeContext: ActiveContext): Task[List[ClassType[Any]]] = {
      obj
        .get(types.`@type`) match {
        case Some(json) =>
          json.array match {
            case Some(array) =>
              Task.gather(
                array.map(
                  json =>
                    json.obj
                      .map { obj =>
                        val expandedJson = activeContext.expandKeys(obj)
                        if (expandedJson.size == 1)
                          activeContext
                            .extractId(expandedJson)
                            .map { iri =>
                              graph.ns.datatypes
                                .get(iri)
                                .orElse(graph.ns.ontologies.get(iri))
                                .orElse(graph.ns.properties.get(iri))
                                .map(Task.evalOnce(_))
                                .getOrElse(fetchClassType(iri))
                            }
                            .getOrElse(throw FromJsonException("@type object without @id"))
                        else toClasstype(obj)
                      }
                      .orElse(json.string.map(activeContext.expandIri).map { iri =>
                        graph.ns.datatypes
                          .get(iri)
                          .orElse(graph.ns.ontologies.get(iri))
                          .orElse(graph.ns.properties.get(iri))
                          .map(Task.evalOnce(_))
                          .getOrElse(fetchClassType(iri))
                      })
                      .getOrElse(Task.raiseError(FromJsonException("nested types not allowed")))))
            case None =>
              json.string
                .map(activeContext.expandIri)
                .map(
                  iri =>
                    graph.ns.datatypes
                      .get(iri)
                      .orElse(graph.ns.ontologies.get(iri))
                      .orElse(graph.ns.properties.get(iri))
                      .map(Task.evalOnce(_))
                      .getOrElse(fetchClassType(iri)))
                .getOrElse(Task.raiseError(FromJsonException("nested types not allowed")))
                .map(List(_))
          }
        case None => Task.now(List())
      }
    }

    def fetchOntology(iri: String)(implicit activeContext: ActiveContext): Task[Ontology]      = fetch(iri)(toOntology)
    def fetchProperty(iri: String)(implicit activeContext: ActiveContext): Task[Property]      = fetch(iri)(toProperty)
    def fetchClassType(iri: String)(implicit activeContext: ActiveContext): Task[ClassType[_]] = fetch(iri)(toClasstype)
    private def fetch[T](iri: String)(cb: JsonObject => Task[T]): Task[T] = {
      println(s"fetch ${iri}")
      Task
        .fromTry(httpClient.getResource(iri) { content =>
          Parse.parseOption(content).flatMap(_.obj).map(cb).getOrElse(throw FromJsonException("could not parse"))
        })
        .flatten
    }

    def extractContext(obj: JsonObject)(implicit activeContext: ActiveContext): Task[ActiveContext] = {
      obj(types.`@context`)
        .map { json =>
          if (json.isNull) Seq[Json]()
          else
            json.array
              .map(_.toSeq)
              .orElse(Some(Seq(json)))
              .get
        }
        .map { values =>
          values.foldLeft(Task.now(activeContext)) {
            case (activeContext, json) =>
              json.string
                .map(iri => throw FromJsonException("remote context not yet implemented"))
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
                                .map(value =>
                                  activeContext.copy(`@prefix` = activeContext.`@prefix` + (expKey -> value)))
                                .map(Task.now)
                                .orElse {
                                  json.obj
                                    .map { value =>
                                      graph.ns.properties
                                        .get(expKey)
                                        .map(Task.now)
                                        .getOrElse {
                                          fetchProperty(expKey)(activeContext)
                                        }
                                        .flatMap { property =>
                                          val newactiveContext = activeContext.copy(
                                            properties = activeContext.properties + (property -> ActiveProperty()))
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
                .get
          }
        }
        .getOrElse(Task.now(activeContext))
    }

    private def bkv(property: Property)(activeContextTask: Task[ActiveContext],
                                        kv: (String, Json)): Task[ActiveContext] = {
      activeContextTask.flatMap { activeContext =>
        val key   = kv._1
        val value = kv._2
        val pMod  = activeContext.properties(property)
        activeContext.expandIri(key) match {
          case types.`@type` =>
            value.string
              .map(activeContext.expandIri(_))
              .map { expKey =>
                graph.ns.classtypes
                  .get(expKey)
                  .map(Task.now)
                  .getOrElse {
                    fetchClassType(expKey)(activeContext)
                  }
                  .map { ct =>
                    activeContext.copy(properties =
                      activeContext.properties + (property -> pMod.copy(`@type` = ct :: Nil)))
                  }
              }
              .orElse {
                value.obj
                  .map(toClasstype(_)(activeContext)
                    .map { ct =>
                      activeContext.copy(properties =
                        activeContext.properties + (property -> pMod.copy(`@type` = ct :: Nil)))
                    })
              }
              .getOrElse(throw FromJsonException(s"@type has unexpected value $value"))
          case types.`@vocab` =>
            value.string
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
            value.string
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
            value.string
              .map(activeContext.expandIri(_))
              .flatMap(`@container`.apply)
              .map(iri =>
                activeContext.copy(properties =
                  activeContext.properties + (property -> pMod.copy(`@container` = iri :: Nil))))
              .map(Task.now)
              .getOrElse(Task.raiseError(FromJsonException(s"@container is not a string")))
          case types.`@base` =>
            value.string
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
  lazy val decode = new decode {}
}
