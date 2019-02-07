package lspace.codec

import java.time.{Instant, LocalDate, LocalTime}

import lspace.NS.types
import lspace.codec.exception.ToJsonException
import lspace.librarian.datatype._
import lspace.librarian.structure._
import lspace.types.vector.Geometry
import squants.time.Time

import scala.collection.immutable.ListSet

trait Encode[Json, JsonObject] {

  type AC   = ActiveContext[Json, JsonObject]
  type AP   = ActiveProperty[Json, JsonObject]
  type JOIP = JsonObjectInProgress[Json, JsonObject]
  type JIP  = JsonInProgress[Json, JsonObject]
  def getNewActiveContext: AC
  def getNewActiveProperty: AP

  implicit def jsonObjectToJson(json: JsonObject): Json

  implicit def textToJson(text: String): Json
  implicit def boolToJson(boolean: Boolean): Json
  implicit def intToJson(int: Int): Json
  implicit def doubleToJson(double: Double): Json
  implicit def longToJson(long: Long): Json
  implicit def geoToJson(geo: Geometry): Json

  implicit def mapToJson(map: Map[String, Json]): Json
  implicit def mapToJsonObject(map: Map[String, Json]): JsonObject
  implicit def listToJson(list: List[Json]): Json
  implicit def tuple2ToJson(tuples: (Json, Json)): Json
  implicit def tuple3ToJson(tuples: (Json, Json, Json)): Json
  implicit def tuple4ToJson(tuples: (Json, Json, Json, Json)): Json
  implicit def tuple2ListToJson(tuples: List[(Json, Json)]): Json

  def apply[T <: Node](node: Node): String

  implicit class WithIriString(iri: String)(implicit activeContext: AC) {
    lazy val compact: String = activeContext.compactIri(iri)
  }

  def fromNode(node: Node)(implicit activeContext: AC): JOIP

  def fromEdges(key: Property, edges: List[Edge[_, _]])(implicit activeContext: AC): JIP

  protected def edgeInVToJson[T](edge: Edge[_, T])(implicit activeContext: AC): JIP = {
    fromAny(edge.inV,
            activeContext.properties
              .get(edge.key)
              .flatMap(_.`@type`.headOption)
              .orElse(edge.key.range.headOption))
  }

  private def edgeOutVToJson[T](edge: Edge[_, T])(implicit activeContext: AC): JIP = {
    fromAny(edge.outV, None)(activeContext)
  }

  def fromEdge(edge: Edge[_, _])(implicit activeContext: AC): JOIP

  def fromData(value: Any, expectedType: DataType[_])(implicit activeContext: AC): JIP = {
    expectedType match {
      case label: LiteralType[_]    => fromLiteral(value, label)
      case label: StructuredType[_] => fromStructured(value, label)
    }
  }

  def fromLiteral(value: Any, expectedType: LiteralType[_])(implicit activeContext: AC): JIP = {
    expectedType match {
      case label: BoolType[_] =>
        value match {
          case v: Boolean => new JIP(v)
          case _          => throw ToJsonException(s"boolean expected ${value.getClass} found")
        }
      case label: CalendarType[_] => fromCalendar(value, label)
      case label: NumericType[_]  => fromNumeric(value, label)
      case label: TextType[_]     => fromText(value, label)
    }
  }

  def fromCalendar(value: Any, expectedType: CalendarType[_])(implicit activeContext: AC): JIP = {
    expectedType match {
      case label: DateTimeType[_]  => fromDateTime(value, label)
      case label: LocalDateType[_] => fromDate(value, label)
      case label: LocalTimeType[_] => fromTime(value, label)
    }
  }

  def fromDateTime(value: Any, expectedType: DateTimeType[_])(implicit activeContext: AC): JIP = {
    value match {
      case v: Instant => JsonInProgress(v.toString())
      case _          => throw ToJsonException(s"datetime expected ${value.getClass} found")
    }
  }

  def fromDate(value: Any, expectedType: LocalDateType[_])(implicit activeContext: AC): JIP = {
    value match {
      case v: LocalDate => JsonInProgress(v.toString())
      case _            => throw ToJsonException(s"date expected ${value.getClass} found")
    }
  }

  def fromTime(value: Any, expectedType: LocalTimeType[_])(implicit activeContext: AC): JIP = {
    value match {
      case v: LocalTime => JsonInProgress(v.toString())
      case _ =>
        throw ToJsonException(s"time expected ${value.getClass} found")
    }
  }

  def fromNumeric(value: Any, expectedType: NumericType[_])(implicit activeContext: AC): JIP = {
    expectedType match {
      case label: IntType[_]    => fromInt(value, label)
      case label: DoubleType[_] => fromDouble(value, label)
      case label: LongType[_]   => fromLong(value, label)
    }
  }

  def fromInt(value: Any, expectedType: IntType[_])(implicit activeContext: AC): JIP = {
    value match {
      case v: Int => JsonInProgress(v)
      case _ =>
        throw ToJsonException(s"int expected ${value.getClass} found")
    }
  }

  def fromDouble(value: Any, expectedType: DoubleType[_])(implicit activeContext: AC): JIP = {
    value match {
      case v: Double => JsonInProgress(v)
      case _         => throw ToJsonException(s"double expected ${value.getClass} found")
    }
  }

  def fromLong(value: Any, expectedType: LongType[_])(implicit activeContext: AC): JIP = {
    value match {
      case v: Long => JsonInProgress(v)
      case _       => throw ToJsonException(s"long expected ${value.getClass} found")
    }
  }

  def fromText(value: Any, expectedType: TextType[_])(implicit activeContext: AC): JIP = {
    value match {
      case v: String => JsonInProgress(v)
      case _         => throw ToJsonException(s"string expected ${value.getClass} found")
    }
  }

  def fromStructured(value: Any, expectedType: StructuredType[_])(implicit activeContext: AC): JIP = {
    expectedType match {
      case label: CollectionType[_] => fromCollection(value, label)
      //        case label: ColorType[_] =>
      case label: GeometricType[_] => fromGeometric(value, label)
      case label: QuantityType[_]  => fromQuantity(value, label)
      case label: TupleType[_]     => fromTuple(value, label)
    }
  }

  private def toArray(v: Seq[_], label: Option[ClassType[_]])(implicit activeContext: AC): JIP = {
    val (jsons, ac) = v.foldLeft((List[Json](), activeContext)) {
      case ((r, activeContext), v) =>
        val jip = fromAny(v, label)(activeContext)
        (r :+ jip.json) -> jip.activeContext
    }
    new JIP(jsons)(ac)
  }

  def fromCollection(value: Any, expectedType: CollectionType[_])(implicit activeContext: AC): JIP = {
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
            new JIP(tuples)(ac)
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
  def fromGeometric(value: Any, expectedType: GeometricType[_])(implicit activeContext: AC): JIP = {
    value match {
      case v: Geometry => JsonInProgress(v)
      case _           => throw ToJsonException(s"int expected ${value.getClass} found")
    }
  }

  def fromQuantity(value: Any, expectedType: QuantityType[_])(implicit activeContext: AC): JIP = {
    expectedType match {
      case label: DurationType =>
        value match {
          case v: Time =>
            JsonInProgress(Map("value" -> (v.value: Json), "unit" -> (v.unit.symbol: Json)): Json)
          case _ => throw ToJsonException(s"duration expected ${value.getClass} found")
        }
    }
  }
  def fromTuple(value: Any, expectedType: TupleType[_])(implicit activeContext: AC): JIP = {
    expectedType match {
      case label: Tuple2Type[_, _] =>
        value match {
          case (v1, v2) =>
            val jip  = fromAny(v1, label._1stRange.headOption)
            val jip2 = fromAny(v2, label._2ndRange.headOption)(jip.activeContext)
            new JIP((jip.json, jip2.json))(jip2.activeContext)
          case _ => throw ToJsonException(s"tuple2 expected ${value.getClass} found")
        }
      case label: Tuple3Type[_, _, _] =>
        value match {
          case (v1, v2, v3) =>
            val jip  = fromAny(v1, label._1stRange.headOption)
            val jip2 = fromAny(v2, label._2ndRange.headOption)(jip.activeContext)
            val jip3 = fromAny(v3, label._3rdRange.headOption)(jip2.activeContext)
            new JIP((jip.json, jip2.json, jip3.json))(jip3.activeContext)
          case _ => throw ToJsonException(s"tuple3 expected ${value.getClass} found")
        }
      case label: Tuple4Type[_, _, _, _] =>
        value match {
          case (v1, v2, v3, v4) =>
            val jip  = fromAny(v1, label._1stRange.headOption)
            val jip2 = fromAny(v2, label._2ndRange.headOption)(jip.activeContext)
            val jip3 = fromAny(v3, label._3rdRange.headOption)(jip2.activeContext)
            val jip4 = fromAny(v4, label._4rdRange.headOption)(jip3.activeContext)
            new JIP((jip.json, jip2.json, jip3.json, jip4.json))(jip4.activeContext)
          case _ => throw ToJsonException(s"tuple4 expected ${value.getClass} found")
        }
    }
  }

  def fromAny(value: Any, expectedType: Option[ClassType[_]] = None)(implicit activeContext: AC): JIP = {
    value match {
      case resource: IriResource =>
        resource match {
          case value: Value[_] =>
            if (expectedType.contains(value.label)) {
              fromData(value.value, value.label)
            } else {
              val jip = fromData(value.value, value.label)
              JsonInProgress(Map(types.`@value` -> jip.json, types.`@type` -> (value.label.iri.compact: Json)): Json)(
                jip.activeContext
              )
            }
          case node: Node =>
            if (node.iri.nonEmpty) JsonInProgress(node.iri: Json)(activeContext)
            else {
              val joip = fromNode(node)
              JsonInProgress(joip.json: Json)(joip.activeContext)
            }
          case edge: Edge[_, _] =>
            if (edge.iri.nonEmpty) JsonInProgress(edge.iri: Json)(activeContext)
            else {
              val joip = fromEdge(edge)
              JsonInProgress(joip.json: Json)(joip.activeContext)
            }
          case iriResource: IriResource => JsonInProgress(iriResource.iri: Json)(activeContext)
        }
      case _ =>
        val label = ClassType.valueToOntologyResource(value)
        if (expectedType.contains(label)) {
          fromData(value, label)
        } else {
          val jip = fromData(value, label)
          JsonInProgress(Map(types.`@value` -> jip.json, types.`@type` -> (label.iri.compact: Json)): Json)(
            jip.activeContext)
        }
    }
  }

  /**
    * ontology to json, TODO: add and return @context
    * @param ontology
    * @return
    */
  def fromOntology(ontology: Ontology)(implicit activeContext: AC): JOIP = {
    val jsProperties = Seq[Option[(String, Json)]](
      Some(types.`@id`   -> (ontology.iri.compact: Json)),
      Some(types.`@type` -> (types.`@class`: Json)),
      ontology.base.map(uri => types.`@base` -> (uri.compact: Json)),
      if (ontology.label.nonEmpty)
        Some(types.`@label` -> (ontology.label.mapValues[Json](t => t): Json))
      else None,
      if (ontology.comment.nonEmpty)
        Some(types.`@comment` -> (ontology.comment.mapValues[Json](t => t): Json))
      else None,
      if (ontology.extendedClasses.nonEmpty)
        Some(types.`@extends` -> (ontology.extendedClasses.map(o => o.iri.compact: Json): Json))
      else None
    ).flatten.toMap ++
      (ontology.properties.toList match {
        case List()         => Map[String, Json]()
        case List(property) => Map(types.`@properties` -> (property.iri.compact: Json))
        case properties =>
          Map(types.`@properties` -> (properties.map(key => key.iri.compact: Json): Json))
      })

    new JOIP(jsProperties: JsonObject)(activeContext)
  }

  /**
    * property to json, TODO: add and return @context
    * @param key
    * @return
    */
  def fromProperty(key: Property)(implicit activeContext: AC): JOIP = {

    val jsProperties = Seq(
      Some(types.`@id`   -> (key.iri.compact: Json)),
      Some(types.`@type` -> (types.`@property`: Json)),
      if (key.label.nonEmpty) Some(types.`@label` -> (key.label.mapValues[Json](t => t): Json))
      else None,
      if (key.comment.nonEmpty) Some(types.`@comment` -> (key.comment.mapValues[Json](t => t): Json))
      else None,
      if (key.container.isDefined)
        Some(types.`@container` -> (List(key.container.get: Json): Json))
      else None,
      if (key.extendedClasses.nonEmpty)
        Some(types.`@extends` -> (key.extendedClasses.map(o => (o.iri.compact: Json)): Json))
      else None
    ).flatten.toMap ++
      (key.range.toList match {
        case List()         => Map[String, Json]()
        case List(dataType) => Map(types.`@range` -> (dataType.iri.compact: Json))
        case dataTypes =>
          Map(types.`@range` -> (dataTypes.map(dataType => (dataType.iri.compact: Json)): Json))
      }) ++
      (key.properties.toList match {
        case List()         => Map[String, Json]()
        case List(property) => Map(types.`@property` -> (property.iri.compact: Json))
        case properties =>
          Map(types.`@property` -> (properties.map(key => key.iri.compact: Json): Json))
      })

    new JOIP(jsProperties: JsonObject)(activeContext)
  }

  def fromDataType(dataType: DataType[_])(implicit activeContext: AC): JOIP = {
    val jsProperties = Seq(
      Some(types.`@id`   -> (dataType.iri: Json)),
      Some(types.`@type` -> (types.`@datatype`: Json)),
      if (dataType.label.nonEmpty)
        Some(types.`@label` -> (dataType.label.mapValues[Json](t => t): Json))
      else None,
      if (dataType.comment.nonEmpty)
        Some(types.`@comment` -> (dataType.comment.mapValues[Json](t => t): Json))
      else None,
      if (dataType.extendedClasses.nonEmpty)
        Some(types.`@extends` -> (dataType.extendedClasses.map[Json, List[Json]](o => o.iri: Json): Json))
      else None
    ).flatten.toMap ++
      (dataType.properties.toList match {
        case List() => Map[String, Json]()
        //        case List(property) => Map(types.properties -> Json.jString(property.iri))
        case properties =>
          Map(types.`@properties` -> (properties.map(key => key.iri: Json): Json))
      })

    val (oProperty, newActiveContext) = dataType match {
      case dt: CollectionType[_] =>
        dt match {
          case dt: ListType[_] =>
            val jip = ctListToJson(dt.valueRange)
            if (dt.valueRange.isEmpty || jip.json.toString.isEmpty) Map() -> jip.activeContext
            else Map(CollectionType.keys.valueRange.iri -> jip.json) -> jip.activeContext
          case dt: ListSetType[_] =>
            val jip = ctListToJson(dt.valueRange)
            if (dt.valueRange.isEmpty || jip.json.toString.isEmpty) Map() -> jip.activeContext
            else Map(CollectionType.keys.valueRange.iri -> jip.json) -> jip.activeContext
          case dt: SetType[_] =>
            val jip = ctListToJson(dt.valueRange)
            if (dt.valueRange.isEmpty || jip.json.toString.isEmpty) Map() -> jip.activeContext
            else Map(CollectionType.keys.valueRange.iri -> jip.json) -> jip.activeContext
          case dt: VectorType[_] =>
            val jip = ctListToJson(dt.valueRange)
            if (dt.valueRange.isEmpty || jip.json.toString.isEmpty) Map() -> jip.activeContext
            else Map(CollectionType.keys.valueRange.iri -> jip.json) -> jip.activeContext
          case dt: MapType[_, _] =>
            val jip  = ctListToJson(dt.keyRange)
            val jip2 = ctListToJson(dt.valueRange)(jip.activeContext)
            if ((dt.keyRange.isEmpty || jip.json.toString.isEmpty) && (dt.valueRange.isEmpty || jip2.json.toString.isEmpty))
              Map() -> jip2.activeContext
            else if (dt.keyRange.isEmpty || jip.json.toString.isEmpty)
              Map(CollectionType.keys.valueRange.iri -> jip2.json) -> jip2.activeContext
            else if (dt.valueRange.isEmpty || jip2.json.toString.isEmpty)
              Map(MapType.keys.keyRange.iri -> jip.json) -> jip2.activeContext
            else
              Map(MapType.keys.keyRange.iri -> jip.json, CollectionType.keys.valueRange.iri -> jip2.json) -> jip2.activeContext
        }
      case _ => Map() -> activeContext
    }
    JsonObjectInProgress(jsProperties ++ oProperty: JsonObject)(newActiveContext)
  }

  def ctListToJson(l: List[ClassType[_]])(implicit activeContext: AC): JIP = {
    if (l.lengthCompare(1) == 0 && l.head.iri.isEmpty) JsonInProgress("": Json)
    else
      l.foldLeft[(List[Json], AC)](List() -> activeContext) {
        case ((l, activeContext), c) =>
          c match {
            case o: Ontology =>
              activeContext.compactIri(o) match { case (key, activeContext) => (l :+ (key: Json)) -> activeContext }
            case p: Property =>
              activeContext.compactIri(p) match { case (key, activeContext) => (l :+ (key: Json)) -> activeContext }
            case d: DataType[_] =>
              d match {
                case d: CollectionType[_] =>
                  fromDataType(d)(activeContext) match {
                    case jip: JOIP =>
                      (l :+ (jip.json: Json)) -> jip.activeContext
                  }
                case _ =>
                  activeContext.compactIri(d) match {
                    case (key, activeContext) => (l :+ (key: Json)) -> activeContext
                  }
              }
          }
      } match { case (l, activeContext) => JsonInProgress(l: Json)(activeContext) }
  }
}
