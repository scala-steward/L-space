package lspace.codec

import java.time.{Instant, LocalDate, LocalTime}

import lspace.NS.types
import lspace.codec.exception.ToJsonException
import lspace.datatype._
import lspace.structure._
import lspace.types.vector.Geometry
import squants.time.Time

import scala.collection.immutable.{ListMap, ListSet}

object Encoder {
//  type Aux[Out] = Encoder { type Json = Out }
  def apply[Json0](implicit baseEncoder0: NativeTypeEncoder.Aux[Json0]): Encoder = new Encoder {
    type Json = Json0
    implicit val baseEncoder: NativeTypeEncoder.Aux[Json] = baseEncoder0
  }
}
trait Encoder {
  type Json
  implicit def baseEncoder: NativeTypeEncoder.Aux[Json]

//  implicit def encoder: lspace.codec.Encoder = this
  type AC   = ActiveContext
  type AP   = ActiveProperty
  type JOIP = JsonObjectInProgress[Json]
  type JIP  = JsonInProgress[Json]

  implicit def nullToJson: Json                   = baseEncoder.jNull
  implicit def textToJson(text: String): Json     = baseEncoder.encode(text)
  implicit def boolToJson(boolean: Boolean): Json = baseEncoder.encode(boolean)
  implicit def intToJson(int: Int): Json          = baseEncoder.encode(int)
  implicit def doubleToJson(double: Double): Json = baseEncoder.encode(double)
  implicit def longToJson(long: Long): Json       = baseEncoder.encode(long)
  implicit def geoToJson(geo: Geometry): Json     = baseEncoder.encode(geo)

  implicit def mapToJson(map: Map[String, Json]): Json              = baseEncoder.encode(map)
  implicit def listmapToJson(map: ListMap[String, Json]): Json      = baseEncoder.encode(map)
  implicit def listToJson(list: List[Json]): Json                   = baseEncoder.encode(list)
  implicit def tuple2ToJson(tuples: (Json, Json)): Json             = baseEncoder.encode(tuples)
  implicit def tuple3ToJson(tuples: (Json, Json, Json)): Json       = baseEncoder.encode(tuples)
  implicit def tuple4ToJson(tuples: (Json, Json, Json, Json)): Json = baseEncoder.encode(tuples)
  implicit def tuple2ListToJson(tuples: List[(Json, Json)]): Json   = baseEncoder.encodeTupleList(tuples)

  def jsonToNoSpacesString(json: Json): String = baseEncoder.jsonToNoSpacesString(json)

  implicit class WithT[T](v: T)(implicit f: T => Json) {
    def asJson = f(v)
  }

  implicit class WithEJson(json: Json) {
    def noSpaces: String = jsonToNoSpacesString(json)
  }

  def apply[T <: Node](node: Node): String = fromNode(node)(ActiveContext()).withContext.noSpaces

  implicit class WithIriString(iri: String)(implicit activeContext: AC) {
    lazy val compact: String = activeContext.compactIri(iri)
  }

  def fromNode(node: Node)(implicit activeContext: AC): JOIP = {
    node match {
      case node: Node if node.labels.contains(DataType.ontology) =>
        fromDataType(DataType.datatypes.get(node.iri).get) //(DataType.build(node))
      case node: Node if node.labels.contains(Property.ontology) =>
        fromProperty(Property.properties.get(node.iri).get) //(Property.build(node))
      case node: Node if node.labels.contains(Ontology.ontology) =>
        fromOntology(Ontology.ontologies.get(node.iri).get) //(Ontology.build(node))
      case nodeResource =>
        nodeResource.labels.foldLeft(List[Json]() -> activeContext) {
          case ((iris, activeContext), tpe) =>
            val (iri, newBuilder) = activeContext.compactIri(tpe)
            (iris :+ textToJson(iri)) -> newBuilder
        } match {
          case (typeIris, activeContext) =>
            val iri = nodeResource.iri
            //                if (nodeResource.iri.nonEmpty) nodeResource.iri
            //                else nodeResource.graph.iri + "/" + nodeResource.id
            val (edges, newAc) = addEdges(nodeResource)(activeContext)
            JsonObjectInProgress[Json](
              List[(String, Json)]() ++ List(iri).filter(_.nonEmpty).map(types.`@id` -> textToJson(_)) ++ List({
                val iris = nodeResource.iris
                if (iris.toList.lengthCompare(1) == 0) List(types.`@ids` -> textToJson(iris.head))
                else if (iris.nonEmpty)
                  List(types.`@ids` -> (nodeResource.iris.toList.map(textToJson).asJson))
                else List()
              }: _*) ++ {
                if (typeIris.lengthCompare(1) == 0) Seq(types.`@type` -> typeIris.head)
                else if (typeIris.nonEmpty) Seq(types.`@type` -> (typeIris.asJson))
                else List()
              } ++ edges)(newAc)
        }
    }
  }

//  implicit class WithDictionary(jsonIP: JOIP) {
//    implicit val activeContext = jsonIP.activeContext
  def addEdges(resource: Resource[_])(implicit activeContext: AC): (List[(String, Json)], AC) = {
    resource
      .outEMap()
      .filterNot { case (key, properties) => Graph.baseKeys.contains(key) }
      .foldLeft((Map[String, Json](), activeContext)) {
        case ((result, activeContext), (key, edges: List[Edge[_, _]])) if edges.nonEmpty =>
          val (compactIri, newActiveContext) = activeContext.compactIri(key)
          val jip                            = fromEdges(key, edges)(newActiveContext)
          result + (compactIri -> jip.json) -> jip.activeContext
        //TODO: add local @context if parent context already has other overrides
        case ((result, activeContext), (key, edges: List[Edge[_, _]])) => result -> activeContext
      } match {
      case (result, activeContext) =>
        result.toList -> activeContext
    }
  }
//  }

  def fromEdges(key: Property, edges: List[Edge[_, _]])(implicit activeContext: AC): JIP = {

    val labelO: Option[ClassType[_]] =
      edges.groupBy(p => p.to.labels.headOption).toList.maxBy(_._2.size)._1

    val newActiveContext =
      if (edges.lengthCompare(4) == 1 && labelO.isDefined &&
          activeContext.definitions.get(key.iri).exists(_.`@type`.isEmpty) && {
            if (labelO.exists(l => key.range().headOption.contains(l))) false
            else edges.size / edges.size.toDouble > 0.8
          }) {
        activeContext.copy(
          definitions = activeContext.definitions + activeContext.definitions
            .get(key.iri)
            .map(ap => key.iri -> ap)
            .getOrElse(key.iri -> ActiveProperty(`@type` = labelO.get :: Nil, property = key)))
      } else activeContext

    edges match {
      case null | List() => JsonInProgress[Json](nullToJson)
      case List(property) /*if key.container.isEmpty*/ =>
        edgeToAsJson(property)(activeContext)
      case edges =>
        edges.foldLeft(List[Json]() -> activeContext) {
          case ((result, activeContext), edge) =>
            val jip = edgeToAsJson(edge)(activeContext)
            (result :+ jip.json) -> jip.activeContext
        } match {
          case (result, activeContext) => JsonInProgress[Json](result.asJson)(activeContext)
        }
    }
  }

  protected def edgeToAsJson[T](edge: Edge[_, T])(implicit activeContext: AC): JIP = {
    fromAny(edge.inV,
            activeContext.definitions
              .get(edge.key.iri)
              .flatMap(_.`@type`.headOption)
              .orElse(edge.key.range().headOption))
  }

  private def edgeFromAsJson[T](edge: Edge[_, T])(implicit activeContext: AC): JIP = {
    fromAny(edge.outV, None)(activeContext)
  }

  def fromEdge(edge: Edge[_, _])(implicit activeContext: AC): JOIP = {
    val (keyIri, newActiveContext) = activeContext.compactIri(edge.key)
    val (edges, newAc)             = addEdges(edge)(newActiveContext)
    JsonObjectInProgress[Json](
      List[(String, Json)]() ++ List(edge.iri).filter(_.nonEmpty).map(types.`@id` -> textToJson(_)) ++ List({
        val iris = edge.iris
        if (iris.toList.lengthCompare(1) == 0) List(types.`@ids` -> textToJson(iris.head))
        else if (iris.nonEmpty)
          List(types.`@ids` -> (edge.iris.toList.map(textToJson).asJson))
        else List()
      }: _*) ++ List(types.`@type` -> textToJson(keyIri)))(newAc)
  }

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
            JsonInProgress(Map("value" -> (v.value.asJson), "unit" -> (v.unit.symbol.asJson)).asJson)
          case _ => throw ToJsonException(s"duration expected ${value.getClass} found")
        }
    }
  }
  def fromTuple(value: Any, expectedType: TupleType[_])(implicit activeContext: AC): JIP = {
    val (jsons, newAc) = value
      .asInstanceOf[Product]
      .productIterator
      .toList
      .zip(expectedType.rangeTypes)
      .foldLeft(List[Json]() -> activeContext) {
        case ((jsons, activeContext), (value, types)) =>
          val jip = fromAny(value, types.headOption)(activeContext)
          (jsons :+ jip.json) -> jip.activeContext
      }
//    val json = jsons match {
//      case List(a, b)             => (a, b).asJson
//      case List(a, b, c)          => (a, b, c).asJson
//      case List(a, b, c, d)       => (a, b, c, d).asJson
//      case List(a, b, c, d, e)    => (a, b, c, d, e).asJson
//      case List(a, b, c, d, e, f) => (a, b, c, d, e, f).asJson
//    }

    new JIP(jsons.asJson)(newAc)
//    expectedType match {
//      case label: Tuple2Type[_, _] =>
//        value match {
//          case (v1, v2) =>
//            val jip  = fromAny(v1, label._1stRange.headOption)
//            val jip2 = fromAny(v2, label._2ndRange.headOption)(jip.activeContext)
//            new JIP((jip.json, jip2.json))(jip2.activeContext)
//          case _ => throw ToJsonException(s"tuple2 expected ${value.getClass} found")
//        }
//      case label: Tuple3Type[_, _, _] =>
//        value match {
//          case (v1, v2, v3) =>
//            val jip  = fromAny(v1, label._1stRange.headOption)
//            val jip2 = fromAny(v2, label._2ndRange.headOption)(jip.activeContext)
//            val jip3 = fromAny(v3, label._3rdRange.headOption)(jip2.activeContext)
//            new JIP((jip.json, jip2.json, jip3.json))(jip3.activeContext)
//          case _ => throw ToJsonException(s"tuple3 expected ${value.getClass} found")
//        }
//      case label: Tuple4Type[_, _, _, _] =>
//        value match {
//          case (v1, v2, v3, v4) =>
//            val jip  = fromAny(v1, label._1stRange.headOption)
//            val jip2 = fromAny(v2, label._2ndRange.headOption)(jip.activeContext)
//            val jip3 = fromAny(v3, label._3rdRange.headOption)(jip2.activeContext)
//            val jip4 = fromAny(v4, label._4rdRange.headOption)(jip3.activeContext)
//            new JIP((jip.json, jip2.json, jip3.json, jip4.json))(jip4.activeContext)
//          case _ => throw ToJsonException(s"tuple4 expected ${value.getClass} found")
//        }
//    }
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
              JsonInProgress(Map(types.`@value` -> jip.json, types.`@type` -> (value.label.iri.compact.asJson)).asJson)(
                jip.activeContext
              )
            }
          case node: Node =>
            if (node.iri.nonEmpty) JsonInProgress(node.iri.asJson)(activeContext)
            else {
              val joip = fromNode(node)
              JsonInProgress((ListMap[String, Json]() ++ joip.json).asJson)(joip.activeContext)
            }
          case edge: Edge[_, _] =>
            if (edge.iri.nonEmpty) JsonInProgress(edge.iri.asJson)(activeContext)
            else {
              val joip = fromEdge(edge)
              JsonInProgress((ListMap[String, Json]() ++ joip.json).asJson)(joip.activeContext)
            }
          case iriResource: IriResource => JsonInProgress(iriResource.iri.asJson)(activeContext)
        }
      case _ =>
        val label = ClassType.valueToOntologyResource(value)
        if (expectedType.contains(label)) {
          fromData(value, label)
        } else {
          val jip = fromData(value, label)
          JsonInProgress(Map(types.`@value` -> jip.json, types.`@type` -> (label.iri.compact.asJson)).asJson)(
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
      Some(types.`@id`   -> (ontology.iri.compact.asJson)),
      Some(types.`@type` -> (types.`@class`.asJson)),
//      ontology.base.map(uri => types.`@base` -> (uri.compact.asJson)),
      if (ontology.label().nonEmpty)
        Some(types.`@label` -> ontology.label().mapValues[Json](t => t).asJson)
      else None,
      if (ontology.comment().nonEmpty)
        Some(types.`@comment` -> ontology.comment().mapValues[Json](t => t).asJson)
      else None,
      if (ontology.extendedClasses().nonEmpty)
        Some(types.`@extends` -> ontology.extendedClasses().map(o => o.iri.compact.asJson).asJson)
      else None
    ).flatten ++
      (ontology.properties().toList match {
        case List()         => List[(String, Json)]()
        case List(property) => List(types.`@properties` -> property.iri.compact.asJson)
        case properties =>
          List(types.`@properties` -> properties.map(key => key.iri.compact.asJson).asJson)
      })

    new JOIP(List[(String, Json)]() ++ jsProperties)(activeContext)
  }

  /**
    * property to json, TODO: add and return @context
    * @param key
    * @return
    */
  def fromProperty(key: Property)(implicit activeContext: AC): JOIP = {

    val jsProperties = Seq(
      Some(types.`@id`   -> (key.iri.compact.asJson)),
      Some(types.`@type` -> (types.`@property`.asJson)),
      if (key.label().nonEmpty) Some(types.`@label` -> (key.label().mapValues[Json](t => t).asJson))
      else None,
      if (key.comment().nonEmpty) Some(types.`@comment` -> (key.comment().mapValues[Json](t => t).asJson))
      else None,
//      if (key.container.isDefined)
//        Some(types.`@container` -> (List(key.container.get.asJson).asJson))
//      else None,
      if (key.extendedClasses().nonEmpty)
        Some(types.`@extends` -> (key.extendedClasses().map(o => (o.iri.compact.asJson)).asJson))
      else None
    ).flatten ++
      (key.range().toList match {
        case List()         => List[(String, Json)]()
        case List(dataType) => List(types.`@range` -> (dataType.iri.compact.asJson))
        case dataTypes =>
          List(types.`@range` -> (dataTypes.map(dataType => (dataType.iri.compact.asJson)).asJson))
      }) ++
      (key.properties().toList match {
        case List()         => List[(String, Json)]()
        case List(property) => List(types.`@property` -> (property.iri.compact.asJson))
        case properties =>
          List(types.`@property` -> (properties.map(key => key.iri.compact.asJson).asJson))
      })

    new JOIP(List[(String, Json)]() ++ jsProperties)(activeContext)
  }

  def fromDataType(dataType: DataType[_])(implicit activeContext: AC): JOIP = {
    val jsProperties = Seq(
      Some(types.`@id`   -> (dataType.iri.asJson)),
      Some(types.`@type` -> (types.`@datatype`.asJson)),
      if (dataType.label().nonEmpty)
        Some(types.`@label` -> (dataType.label().mapValues[Json](t => t).asJson))
      else None,
      if (dataType.comment().nonEmpty)
        Some(types.`@comment` -> (dataType.comment().mapValues[Json](t => t).asJson))
      else None,
      if (dataType.extendedClasses().nonEmpty)
        Some(types.`@extends` -> (dataType.extendedClasses().map[Json, List[Json]](o => o.iri.asJson).asJson))
      else None
    ).flatten ++
      (dataType.properties().toList match {
        case List() => List[(String, Json)]()
        //        case List(property) => Map(types.properties -> Json.jString(property.iri))
        case properties =>
          List(types.`@properties` -> (properties.map(key => key.iri.asJson).asJson))
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
    JsonObjectInProgress[Json](List[(String, Json)]() ++ jsProperties ++ oProperty)(newActiveContext)
  }

  def ctListToJson(l: List[ClassType[_]])(implicit activeContext: AC): JIP = {
    if (l.lengthCompare(1) == 0 && l.head.iri.isEmpty) JsonInProgress("".asJson)
    else
      l.foldLeft[(List[Json], AC)](List() -> activeContext) {
        case ((l, activeContext), c) =>
          c match {
            case o: Ontology =>
              activeContext.compactIri(o) match { case (key, activeContext) => (l :+ (key.asJson)) -> activeContext }
            case p: Property =>
              activeContext.compactIri(p) match { case (key, activeContext) => (l :+ (key.asJson)) -> activeContext }
            case d: DataType[_] =>
              d match {
                case d: CollectionType[_] =>
                  fromDataType(d)(activeContext) match {
                    case jip: JOIP =>
                      (l :+ (ListMap[String, Json]() ++ jip.json).asJson) -> jip.activeContext
                  }
                case _ =>
                  activeContext.compactIri(d) match {
                    case (key, activeContext) => (l :+ (key.asJson)) -> activeContext
                  }
              }
          }
      } match { case (l, activeContext) => JsonInProgress(l.asJson)(activeContext) }
  }
}
