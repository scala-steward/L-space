package lspace.parse.json

import java.time.{Instant, LocalDate, LocalTime}

import argonaut._
import Argonaut._
import lspace.NS
import lspace.NS.types
import lspace.librarian.datatype._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure._
import lspace.parse.{LDContextBuilder, LDGraphBuilder}
import lspace.parse.util.{FromJsonException, HttpClient, HttpClientImpl}
import lspace.types.vector.{Geometry, Point}

import scala.collection.immutable.{HashMap, ListMap, ListSet}
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object JsonLD {
  def apply(graph: Graph): JsonLD = new JsonLD(graph)
}
class JsonLD(graph: Graph) {

  val httpClient: HttpClient = HttpClientImpl

  sealed trait ResourceOrClassType[T]
  object ResourceOrClassType {
    implicit def resource[T]: ResourceOrClassType[Resource[T]] =
      new ResourceOrClassType[Resource[T]] {}
    implicit def classtype[T]: ResourceOrClassType[ClassType[T]] =
      new ResourceOrClassType[ClassType[T]] {}
  }
  //  def toJson[T: ResourceOrClassType](value: T, builder: LDContextBuilder = LDContextBuilder()): (Json, LDContextBuilder) = {
  //    value match {
  //      case node: Node =>
  //      case edge: Edge[_, _] =>
  //      case value: Value[_] =>
  //      case ontology: Ontology =>
  //      case property: Property =>
  //    }
  //  }

  def addContext(builder: LDContextBuilder, json: JsonObject): JsonObject = {
    val (newBuilder, result) = builder.typeMods.foldLeft((builder, ListMap[String, Json]())) {
      case ((builder, result), (key, ct)) =>
        val (keyIri, newBuilder) = builder.compactIri(key)
        val (json, newBuilder2) = ct match {
          case ct: CollectionType[_] =>
            dataTypeToJson(ct, newBuilder) match { case (key, value) => Json.jObject(key) -> value }
          case _ => newBuilder.compactIri(ct) match { case (key, value) => key.asJson -> value }
        }
        newBuilder2 -> (result ++ Map(keyIri -> Map(types.`@type` -> json).asJson))
    }
    JsonObject.fromTraversableOnce(
      Map(types.`@context` -> Json.jObject(JsonObject.fromTraversableOnce(newBuilder.context.map {
        case (iri, prefix) => prefix -> iri.asJson
      }.toList ++ result.toList))) ++ json.toMap)
  }

  def nodeToJsonWithContext(node: Node, builder: LDContextBuilder = LDContextBuilder()): (Json, LDContextBuilder) = {
    val (json, newBuilder) = nodeToJson(node, builder)
    Json.jObject(addContext(newBuilder, json)) -> newBuilder
  }

  /**
    * this method can be overridden when extending the parser and add additional
    * Scala object representations (e.g. Ontology and Traversal classes)
    * and corresponding parsers. Add a wildcard match-case which points to this method to re-use the base matchers.
    * E.g. case _ => super.nodeResourceToJson(nodeResource, builder)
    * @param node
    * @return
    */
  def nodeToJson(node: Node, builder: LDContextBuilder = LDContextBuilder()): (JsonObject, LDContextBuilder) =
    node match {
      //        case datatype: DataType[_] =>
      case node: Node if node.labels.contains(DataType.ontology) =>
        dataTypeToJson(DataType.apply(node), builder)
      case node: Node if node.labels.contains(Property.ontology) =>
        propertyToJson(Property.apply(node), builder)
      case node: Node if node.labels.contains(Ontology.ontology) =>
        ontologyToJson(Ontology.apply(node), builder)
      //        case traversal: Traversal[Any, Any, Any, HNil] => traversalToJson(traversal) -> builder
      case nodeResource =>
        nodeResource.labels.foldLeft(List[Json]() -> builder) {
          case ((iris, builder), tpe) =>
            val (iri, newBuilder) = builder.compactIri(tpe)
            (iris :+ Json.jString(iri)) -> newBuilder
        } match {
          case (typeIris, builder) =>
            val jsProperties =
              (Seq(nodeResource.iri).filter(_.nonEmpty).map(types.`@id` -> Json.jString(_)) ++ {
                val iris = nodeResource.iris
                if (iris.toList.lengthCompare(1) == 0) Seq(types.`@ids` -> Json.jString(iris.head))
                else if (iris.nonEmpty)
                  Seq(types.`@ids` -> nodeResource.iris.toList.map(Json.jString).asJson)
                else Seq()
              } ++ {
                if (typeIris.lengthCompare(1) == 0) Seq(types.`@type` -> typeIris.head)
                else if (typeIris.nonEmpty) Seq(types.`@type` -> typeIris.asJson)
                else Seq()
              }).toMap
            addEdgesToJson(nodeResource, builder) match {
              case (result, builder) =>
                JsonObject.fromTraversableOnce(jsProperties ++ result.toMap) -> builder
            }
        }
    }

  def addEdgesToJson[T](resource: Resource[T],
                        builder: LDContextBuilder = LDContextBuilder()): (JsonObject, LDContextBuilder) = {
    resource
      .outEMap()
      .filterNot { case (key, properties) => Graph.baseKeys.contains(key) }
      .foldLeft((Map[String, Json](), builder)) {
        case ((result, builder), (key, properties: List[Edge[_, _]])) =>
          val (labelO, edges) =
            properties.groupBy(p => p.to.labels.headOption).toList.maxBy(_._2.size)
          val newBuilder =
            if (properties.lengthCompare(0) == 1 && labelO.isDefined && {
                  if (labelO.exists(l => key.range.headOption.contains(l))) false
                  else edges.size / properties.size.toDouble > 0.8
                }) {
              builder.copy(
                typeMods = builder.typeMods + builder.typeMods
                  .get(key)
                  .map(ct => key -> ct)
                  .getOrElse(key -> labelO.get))
            } else builder
          newBuilder.compactIri(key) match {
            case (compactIri, builder) =>
              (properties.asInstanceOf[List[Edge[_, _]]] match {
                case null | List() => Json.jNull -> builder
                case List(property) if key.container.isEmpty =>
                  edgeInVToJson(property, builder) match {
                    case (result, builder) => result -> builder
                  }
                case properties =>
                  properties.foldLeft(List[Json]() -> builder) {
                    case ((result, builder), property) =>
                      edgeInVToJson(property, builder) match {
                        case (result2, builder) => (result :+ result2) -> builder
                      }
                  } match {
                    case (result, builder) => result.asJson -> builder
                  }
              }) match {
                case (jsValue, builder) =>
                  result + (compactIri -> jsValue) -> builder
              }
          }
      } match {
      case (result, builder) =>
        JsonObject.fromTraversableOnce(result) -> builder
    }
  }

  private def edgeToJson[T](edge: Edge[_, T],
                            builder: LDContextBuilder = LDContextBuilder()): (Json, LDContextBuilder) = {
    edgeInVToJson(edge, builder) match {
      case (to, builder) =>
        edgeOutVToJson(edge, builder) match {
          case (from, builder) =>
            addEdgesToJson(edge, builder) match {
              case (result, builder) =>
                val (cKeyIri, newBuilder) = builder.compactIri(edge.key)
                (Map(types.`@to` -> to, types.`@from` -> from, types.`@type` -> cKeyIri.asJson) ++ result.toMap asJson) -> newBuilder
            }
        }
    }
  }

  private def edgeInVToJson[T](edge: Edge[_, T],
                               builder: LDContextBuilder = LDContextBuilder()): (Json, LDContextBuilder) = {
    anyToJson(edge.inV, builder.typeMods.get(edge.key).map(List(_)).getOrElse(edge.key.range.toList), builder)
  }

  private def edgeOutVToJson[T](edge: Edge[_, T],
                                builder: LDContextBuilder = LDContextBuilder()): (Json, LDContextBuilder) = {
    anyToJson(edge.outV, List(), builder)
  }

  def anyToJson(value: Any,
                eCT: List[ClassType[_]] = List(),
                builder: LDContextBuilder = LDContextBuilder()): (Json, LDContextBuilder) = {
    value match {
      case v: Resource[_] =>
        v match {
          case v: Node =>
            if (v.iri.nonEmpty) {
              eCT.size match {
                case _ =>
                  if (eCT.headOption.exists(ct =>
                        ct.isInstanceOf[Ontology] ||
                          ct.isInstanceOf[Property] || ct.isInstanceOf[IriType[Node]])) {
                    Json.jString(v.iri).asJson -> builder
                  } else
                    Map(types.`@value` -> Json.jString(v.iri), types.`@type` -> Json.jString(types.`@id`)).asJson -> builder
              }
            } else { //object inc. all out-edges? or pass local-id (long)
              if (builder.iriLessNodes.contains(v.id.toString)) {
                if (eCT.headOption.exists(ct =>
                      ct.isInstanceOf[Ontology] ||
                        ct.isInstanceOf[Property] || ct.isInstanceOf[IriType[Node]])) {
                  Json.jString(s"_:${v.graph.iri}/id/${v.id}").asJson -> builder
                } else
                  Map(types.`@value` -> Json.jString(v.iri), types.`@type` -> Json.jString(types.`@id`)).asJson -> builder
              } else {
                val (obj, newBuilder) =
                  nodeToJson(v, builder.copy(iriLessNodes = builder.iriLessNodes ++ Map(v.id.toString -> v)))
                Json.jObject(obj) -> newBuilder
              }
            } //strip @type when it is redundant w.r.t. the key
          case v: Edge[_, _] =>
            val (json, newBuilder) = edgeToJson(v, builder)
            json -> newBuilder
          //          throw ToJsonException("property --> property not yet supported")
          case v: Value[_] =>
            if (v.iri.nonEmpty) {
              Json.jObject(
                JsonObject.fromTraversableOnce(
                  Map(types.`@id` -> Json.jString(v.iri), types.`@type` -> v.label.iri.asJson))) -> builder
            } else {
              eCT.size match {
                case 0 if v.label.iri == types.`@string` =>
                  valueToJson(v.value)(v.label) -> builder
                case 1 if v.label == eCT.head =>
                  anyToJson(v.value, List(v.label), builder)
                case _ =>
                  val (dataTypeIri, newBuilder) = builder.compactIri(v.label)
                  val (value, newBuilder2)      = anyToJson(v.value, List(v.label), newBuilder)
                  Json.jObject(
                    JsonObject
                      .fromTraversableOnce(Map(types.`@value` -> value, types.`@type` -> dataTypeIri.asJson))) -> newBuilder2
              }
            }
        }
      case v: Map[_, _] =>
        eCT.headOption match {
          case Some(ct: MapType[_, _]) =>
            val (mr, mb) = v.foldLeft((List[(Json, Json)](), builder)) {
              case ((r, builder), (k, v)) =>
                val (kr, kb) = anyToJson(k, ct.keyRange, builder)
                val (vr, vb) = anyToJson(v, ct.valueRange, kb)
                (r :+ (kr, vr)) -> vb
            }
            mr.asJson -> mb
          case _ =>
            val (mr, mb) = v.foldLeft((List[(Json, Json)](), builder)) {
              case ((r, builder), (k, v)) =>
                val (kr, kb) = anyToJson(k, builder = builder)
                val (vr, vb) = anyToJson(v, builder = kb)
                (r :+ (kr, vr)) -> vb
            }
            Json.jObject(
              JsonObject.fromTraversableOnce(
                Map(types.`@value` -> mr.asJson, types.`@container` -> types.`@map`.asJson))) -> mb
        }
      case v: List[_] =>
        eCT.headOption match {
          case Some(ct: ListType[_]) =>
            val (lr, lb) = v.foldLeft((List[Json](), builder)) {
              case ((r, builder), v) =>
                val (vr, vb) = anyToJson(v, ct.valueRange, builder)
                (r :+ vr) -> vb
            }
            lr.asJson -> lb
          case _ =>
            val (lr, lb) = v.foldLeft((List[Json](), builder)) {
              case ((r, builder), v) =>
                val (vr, vb) = anyToJson(v, builder = builder)
                (r :+ vr) -> vb
            }
            Json.jObject(
              JsonObject.fromTraversableOnce(
                Map(types.`@value` -> lr.asJson, types.`@container` -> types.`@list`.asJson))) -> lb
        }
      case v: Set[_] =>
        eCT.headOption match {
          case Some(ct: SetType[_]) =>
            val (lr, lb) = v.foldLeft((List[Json](), builder)) {
              case ((r, builder), v) =>
                val (vr, vb) = anyToJson(v, ct.valueRange, builder)
                (r :+ vr) -> vb
            }
            lr.asJson -> lb
          case _ =>
            val (lr, lb) = v.foldLeft((List[Json](), builder)) {
              case ((r, builder), v) =>
                val (vr, vb) = anyToJson(v, builder = builder)
                (r :+ vr) -> vb
            }
            Json.jObject(
              JsonObject.fromTraversableOnce(
                Map(types.`@value` -> lr.asJson, types.`@container` -> types.`@set`.asJson))) -> lb
        }
      case v: Vector[_] =>
        eCT.headOption match {
          case Some(ct: VectorType[_]) =>
            val (lr, lb) = v.foldLeft((List[Json](), builder)) {
              case ((r, builder), v) =>
                val (vr, vb) = anyToJson(v, ct.valueRange, builder)
                (r :+ vr) -> vb
            }
            lr.asJson -> lb
          case _ =>
            val (lr, lb) = v.foldLeft((List[Json](), builder)) {
              case ((r, builder), v) =>
                val (vr, vb) = anyToJson(v, builder = builder)
                (r :+ vr) -> vb
            }
            Json.jObject(
              JsonObject
                .fromTraversableOnce(Map(types.`@value` -> lr.asJson, types.`@container` -> types.`@vector`.asJson))) -> lb
        }
      case v: Any =>
        val dt = ClassType.valueToOntologyResource(v)
        eCT.size match {
          case 1
              if ClassType.valueToOntologyResource(v) == eCT.head => //compare with compatible types, e.g. Numeric, Temporal
            valueToJson(v)(dt) -> builder
          case _ =>
            Json.jObject(
              JsonObject
                .fromTraversableOnce(Map(types.`@value` -> valueToJson(v)(dt), types.`@type` -> dt.iri.asJson))) -> builder
        }
    }
  }

  private def valueToJson(value: Any)(dt: DataType[_]): Json = value match {
    case v: Ontology if dt.iri == DataType.default.`@class`.iri              => Json.jString(v.iri)
    case v: Property if dt.iri == DataType.default.`@property`.iri           => Json.jString(v.iri)
    case v: DataType[_] if dt.iri == DataType.default.`@datatype`.iri        => Json.jString(v.iri)
    case v: IriResource if dt.iri == DataType.default.`@url`.iri             => Json.jString(v.iri)
    case v: String if dt.iri == DataType.default.`@string`.iri               => Json.jString(v)
    case v: Boolean if dt.iri == DataType.default.`@boolean`.iri             => Json.jBool(v)
    case v: Int if dt.iri == DataType.default.`@int`.iri                     => Json.jNumber(v)
    case v: Double if dt.iri == DataType.default.`@double`.iri               => Json.jNumber(v).get
    case v: Long if dt.iri == DataType.default.`@long`.iri                   => Json.jString(v.toString)
    case v: Instant if dt.iri == DataType.default.`@datetime`.iri            => Json.jString(v.toString())
    case v: LocalDate if dt.iri == DataType.default.`@date`.iri              => Json.jString(v.toString())
    case v: LocalTime if dt.iri == DataType.default.`@time`.iri              => Json.jString(v.toString())
    case v: Geometry if dt.iri == DataType.default.`@geo`.iri                => v.asJson
    case v: Geometry /*Point*/ if dt.iri == DataType.default.`@geopoint`.iri => v.asJson
    //        case v if v.isInstanceOf[Geoshape] => geojsonType.toJson(v.asInstanceOf[DataTypes.GeoT])
  }

  /**
    * ontology to json, TODO: add and return @context
    * @param ontology
    * @return
    */
  def ontologyToJson(ontology: Ontology,
                     builder: LDContextBuilder = LDContextBuilder()): (JsonObject, LDContextBuilder) = {
    val jsProperties = Seq(
      Some(types.`@id`   -> Json.jString(ontology.iri)),
      Some(types.`@type` -> Json.jString(types.`@class`)),
      ontology.base.map(uri => types.`@base` -> Json.jString(uri)),
      if (ontology.label.nonEmpty)
        Some(types.`@label` -> ontology.label.mapValues(Json.jString).asJson)
      else None,
      if (ontology.comment.nonEmpty)
        Some(types.`@comment` -> ontology.comment.mapValues(Json.jString).asJson)
      else None,
      if (ontology.extendedClasses.nonEmpty)
        Some(types.`@extends` -> ontology.extendedClasses.map(o => Json.jString(o.iri)).asJson)
      else None
    ).flatten.toMap ++
      (ontology.properties.toList match {
        case List()         => Map[String, Json]()
        case List(property) => Map(types.`@properties` -> Json.jString(property.iri))
        case properties =>
          Map(types.`@properties` -> properties.map(key => Json.jString(key.iri)).asJson)
      })

    JsonObject.fromTraversableOnce(jsProperties) -> builder
  }

  /**
    * property to json, TODO: add and return @context
    * @param key
    * @return
    */
  def propertyToJson(key: Property, builder: LDContextBuilder = LDContextBuilder()): (JsonObject, LDContextBuilder) = {
    val jsProperties = Seq(
      Some(types.`@id`   -> Json.jString(key.iri)),
      Some(types.`@type` -> Json.jString(types.`@property`)),
      if (key.label.nonEmpty) Some(types.`@label` -> key.label.mapValues(Json.jString).asJson)
      else None,
      if (key.comment.nonEmpty) Some(types.`@comment` -> key.comment.mapValues(Json.jString).asJson)
      else None,
      if (key.container.isDefined)
        Some(types.`@container` -> List(Json.jString(key.container.get)).asJson)
      else None,
      if (key.extendedClasses.nonEmpty)
        Some(types.`@extends` -> key.extendedClasses.map(o => Json.jString(o.iri)).asJson)
      else None
    ).flatten.toMap ++
      (key.range.toList match {
        case List()         => Map[String, Json]()
        case List(dataType) => Map(types.`@range` -> Json.jString(dataType.iri))
        case dataTypes =>
          Map(types.`@range` -> dataTypes.map(dataType => Json.jString(dataType.iri)).asJson)
      }) ++
      (key.properties.toList match {
        case List()         => Map[String, Json]()
        case List(property) => Map(types.`@property` -> Json.jString(property.iri))
        case properties =>
          Map(types.`@property` -> properties.map(key => Json.jString(key.iri)).asJson)
      })

    JsonObject.fromTraversableOnce(jsProperties) -> builder
  }

  def ctListToJson(l: List[ClassType[_]], builder: LDContextBuilder = LDContextBuilder()): (Json, LDContextBuilder) = {
    if (l.lengthCompare(1) == 0 && l.head.iri.isEmpty) "".asJson -> builder
    else
      l.foldLeft[(List[Json], LDContextBuilder)](List() -> builder) {
        case ((l, builder), c) =>
          c match {
            case o: Ontology =>
              builder.compactIri(o) match { case (key, builder) => (l :+ key.asJson) -> builder }
            case p: Property =>
              builder.compactIri(p) match { case (key, builder) => (l :+ key.asJson) -> builder }
            case d: DataType[_] =>
              d match {
                case d: CollectionType[_] =>
                  dataTypeToJson(d, builder) match {
                    case (key, builder) =>
                      (l :+ Json.jObject(key)) -> builder
                  }
                case _ =>
                  builder.compactIri(d) match {
                    case (key, builder) => (l :+ key.asJson) -> builder
                  }
              }
          }
      } match { case (l, builder) => l.asJson -> builder }
  }

  def dataTypeToJson(dataType: DataType[_],
                     builder: LDContextBuilder = LDContextBuilder()): (JsonObject, LDContextBuilder) = {
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

    val (oProperty, newBuilder) = dataType match {
      case dt: CollectionType[_] =>
        dt match {
          case dt: ListType[_] =>
            val (l, newBuilder) = ctListToJson(dt.valueRange, builder)
            if (dt.valueRange.isEmpty || l.string.contains("")) Map() -> newBuilder
            else Map(CollectionType.keys.valueRange.iri -> l) -> newBuilder
          case dt: ListSetType[_] =>
            val (l, newBuilder) = ctListToJson(dt.valueRange, builder)
            if (dt.valueRange.isEmpty || l.string.contains("")) Map() -> newBuilder
            else Map(CollectionType.keys.valueRange.iri -> l) -> newBuilder
          case dt: SetType[_] =>
            val (l, newBuilder) = ctListToJson(dt.valueRange, builder)
            if (dt.valueRange.isEmpty || l.string.contains("")) Map() -> newBuilder
            else Map(CollectionType.keys.valueRange.iri -> l) -> newBuilder
          case dt: VectorType[_] =>
            val (l, newBuilder) = ctListToJson(dt.valueRange, builder)
            if (dt.valueRange.isEmpty || l.string.contains("")) Map() -> newBuilder
            else Map(CollectionType.keys.valueRange.iri -> l) -> newBuilder
          case dt: MapType[_, _] =>
            val (l, newBuilder)   = ctListToJson(dt.keyRange, builder)
            val (l2, newBuilder2) = ctListToJson(dt.valueRange, newBuilder)
            if ((dt.keyRange.isEmpty || l.string.exists(_.isEmpty)) && (dt.valueRange.isEmpty || l2.string
                  .exists(_.isEmpty))) Map() -> newBuilder2
            else if (dt.keyRange.isEmpty || l.string.exists(_.isEmpty))
              Map(CollectionType.keys.valueRange.iri -> l2) -> newBuilder2
            else if (dt.valueRange.isEmpty || l2.string.exists(_.isEmpty))
              Map(MapType.keys.keyRange.iri -> l) -> newBuilder2
            else
              Map(MapType.keys.keyRange.iri -> l, CollectionType.keys.valueRange.iri -> l2) -> newBuilder2
        }
      case _ => Map() -> builder
    }

    JsonObject.fromTraversableOnce(jsProperties ++ oProperty) -> newBuilder
  }

  def parseContext(json: JsonObject, builder: LDGraphBuilder = LDGraphBuilder()): Try[(LDGraphBuilder, JsonObject)] =
    Try {
      //      json match {
      //        case obj: JsObject =>
      json(types.`@context`)
        .map { json =>
          if (json.isNull) Seq[Json]()
          else
            json.array
              .map(_.toSeq)
              .orElse(Some(Seq(json)))
              .get
        }
        .map { values =>
          //      values.map { json =>
          //        json.string.getOrElse(json.obj.getOrElse(throw FromJsonException("nested list or number value?")))
          //      }
          values.foldLeft(builder) {
            case (builder, json) =>
              json.string
                .map(iri => throw FromJsonException("remote context not yet implemented"))
                .orElse {
                  json.obj.map(_.toList.foldLeft(builder) {
                    case (builder, (key, json)) =>
                      builder.expandIri(key) match {
                        case types.`@base` =>
                          json.string
                            .map(iri => builder.expandIri(iri))
                            .map(base => builder.copy(context = builder.context.copy(base = Some(base))))
                            .getOrElse(builder)
                        //                      builder.copy(contexts = builder.contexts.drop(1) :+ builder.contexts.last.copy(base = Some(base)))).getOrElse(builder)
                        case types.`@vocab` =>
                          json.string
                            .map(iri => builder.expandIri(iri))
                            .map(vocab =>
                              //                      builder.copy(contexts = builder.contexts.drop(1) :+ builder.contexts.last.copy(vocab = Some(vocab)))).getOrElse(builder)
                              builder.copy(context = builder.context.copy(vocab = Some(vocab))))
                            .getOrElse(builder)
                        case types.`@language` =>
                          json.string
                            .map(iri => builder.expandIri(iri))
                            .map(language =>
                              //                      builder.copy(contexts = builder.contexts.drop(1) :+ builder.contexts.last.copy(language = Some(language)))).getOrElse(builder)
                              builder.copy(context = builder.context.copy(language = Some(language))))
                            .getOrElse(builder)
                        case expKey =>
                          json.string
                            .map(builder.expandIri(_))
                            .map(value =>
                              builder.copy(context =
                                //                        builder.contexts.drop(1) :+ builder.contexts.last.copy(prefix = builder.contexts.last.prefix + (expKey -> value))))
                                builder.context.copy(prefix = builder.context.prefix + (expKey -> value))))
                            .orElse {
                              json.obj.map {
                                value =>
                                  val property = graph.ns.getProperty(expKey).getOrElse {
                                    httpClient
                                      .getResource(expKey)({ string =>
                                        val json = Parse
                                          .parse(string)
                                          .right
                                          .getOrElse(throw FromJsonException(""))
                                        json.obj
                                          .map(jsonToProperty(_) match {
                                            case Success(r) => r
                                            case Failure(e) => throw e
                                          })
                                          .getOrElse(throw FromJsonException(
                                            s"Error while processing term $key expanded iri $expKey"))
                                      })
                                      .toOption
                                      .getOrElse(throw FromJsonException(
                                        s"Could not fetch $expKey builder.context.prefix = ${builder.context.prefix}"))
                                  }
                                  val newBuilder = builder.copy(
                                    context =
                                      //                            builder.contexts.last.copy(property = builder.contexts.last.property + (property -> LDGraphBuilder.PropertyMod())))
                                      builder.context.copy(
                                        property = builder.context.property + (property -> LDGraphBuilder
                                          .PropertyMod())))
                                  value.toList.foldLeft(newBuilder)(bkv(property))
                              }
                            }
                            .getOrElse(throw FromJsonException(s"@context/$expKey is a string nor an object"))
                      }
                  })
                }
                .get
          }
        }
        .map(_ -> (json - types.`@context`))
        .getOrElse(builder -> json)
    }

  private def bkv(property: Property)(builder: LDGraphBuilder, kv: (String, Json)) = {
    val key   = kv._1
    val value = kv._2
    val pMod  = builder.context.property(property)
    builder.expandIri(key) match {
      case types.`@type` =>
        value.string
          .map(builder.expandIri(_))
          .map { expKey =>
            val ct = graph.ns.getClassType(expKey).getOrElse {
              httpClient
                .getResource(expKey)({ string =>
                  val json = Parse.parse(string).right.getOrElse(throw FromJsonException(""))
                  json.obj
                    .map(jsonToClassType(_) match {
                      case Success(r) => r
                      case Failure(e) => throw e
                    })
                    .getOrElse(throw FromJsonException(s"Error while processing term $key expanded iri $expKey"))
                })
                .toOption
                .getOrElse(throw FromJsonException(s"Could not fetch $expKey"))
            }
            builder.copy(context = builder.context.copy(property =
              builder.context.property + (property -> pMod.copy(tpe = Some(ct)))))
          }
          .orElse {
            value.obj
              .map { obj =>
                jsonToClassType(obj, builder) match {
                  case Success(r) => r
                  case Failure(e) => throw e
                }
              }
              .map { ct =>
                builder.copy(context = builder.context.copy(property =
                  builder.context.property + (property -> pMod.copy(tpe = Some(ct)))))
              }
          }
          .getOrElse(throw FromJsonException(s"@type has unexpected value $value"))
      case types.`@vocab` =>
        value.string
          .map(builder.expandIri(_))
          .map(iri =>
            builder.copy(context = builder.context.copy(property =
              builder.context.property + (property -> pMod.copy(context = pMod.context.copy(vocab = Some(iri)))))))
          .getOrElse(throw FromJsonException(s"@vocab has unexpected value $value"))
      //                                    pMod.context.expandIri(iri)).map(vocab => pMod.copy(context = pMod.context.copy(vocab = Some(vocab)))).getOrElse(pMod)
      case types.`@language` =>
        value.string
          .map(builder.expandIri(_))
          .map(iri =>
            builder.copy(context = builder.context.copy(property =
              builder.context.property + (property -> pMod.copy(context = pMod.context.copy(language = Some(iri)))))))
          .getOrElse(throw FromJsonException(s"@language has unexpected value $value"))
      case types.`@container` =>
        value.string
          .map(builder.expandIri(_))
          .map(iri =>
            builder.copy(context = builder.context.copy(property =
              builder.context.property + (property -> pMod.copy(container = Some(iri))))))
          .getOrElse(throw FromJsonException(s"@container has unexpected value $value"))
      case types.`@base` =>
        value.string
          .map(builder.expandIri(_))
          .map(iri =>
            builder.copy(context = builder.context.copy(property =
              builder.context.property + (property -> pMod.copy(context = pMod.context.copy(base = Some(iri)))))))
          .getOrElse(throw FromJsonException(s"@base has unexpected value $value"))
    }
  }

  private def getIri(obj: Map[String, Json], builder: LDGraphBuilder) =
    obj.get(types.`@id`).flatMap(_.string).map(builder.expandIri(_))

  private def getIris(obj: Map[String, Json], builder: LDGraphBuilder) =
    obj
      .get(types.`@ids`)
      .flatMap(
        json =>
          json.array
            .map(_.flatMap(_.string.orElse(throw FromJsonException("unknown key/iri format"))))
            .orElse(json.string.map(List(_))))
      .getOrElse(List())
      .map(builder.expandIri(_))

  private def getTypes(obj: Map[String, Json], builder: LDGraphBuilder = LDGraphBuilder()): List[Ontology] = {
    obj
      .get(types.`@type`)
      .map(
        json =>
          json.array
            .map(_.map(_.string
              .map(builder.expandIri(_))
              .getOrElse(throw FromJsonException("getTypes: unknown @type key/iri format"))))
            .orElse(json.string.map(builder.expandIri(_)).map(List(_)))
            .getOrElse(List())
            .map { iri =>
              graph.ns
                .getOntology(iri)
                .orElse(httpClient
                  .getResource(iri)({ string =>
                    val json = Parse.parse(string).right.getOrElse(throw FromJsonException(""))
                    json.obj
                      .map(jsonToOntology(_) match {
                        case Success(r) => r
                        case Failure(e) => throw e
                      })
                      .getOrElse(throw FromJsonException(s"Error while processing term $iri expanded iri $iri"))
                  })
                  .toOption)
                .map {
                  case ontology: Ontology => ontology
                  case _                  => throw FromJsonException(s"@extends $iri does not link to a property")
                }
                .getOrElse(throw new Exception(s"Could not get ontology $iri")) /*.map(DetachedGraph.nodes.upsert)*/
          })
      .getOrElse(List())
  }

  private def getLabel(obj: Map[String, Json], builder: LDGraphBuilder): Map[String, String] =
    obj
      .get(types.`@label`)
      .flatMap(
        json =>
          json.obj
            .map(_.toMap.map {
              case (key, json) =>
                key -> json.string.getOrElse(throw FromJsonException("@label value is not a string"))
            })
            .orElse(json.string.map(l => Map("en" -> l))))
      .getOrElse(Map())

  private def getComment(obj: Map[String, Json], builder: LDGraphBuilder): Map[String, String] =
    obj
      .get(types.`@comment`)
      .flatMap(
        json =>
          json.obj
            .map(_.toMap.map {
              case (key, json) =>
                key -> json.string.getOrElse(throw FromJsonException("@comment value is not a string"))
            })
            .orElse(json.string.map(l => Map("en" -> l))))
      .getOrElse(Map())

  def jsonToOntology(
      _obj: JsonObject,
      _builder: LDGraphBuilder = LDGraphBuilder()) /*(getOntology: (String) => Ontology = uri => Ontology(uri))*/
    : Try[Ontology] = Try {
    val (builder, obj) = parseContext(_obj, _builder).get
    val expObj         = obj.toMap.map { case (key, value) => builder.expandIri(key) -> value }

    val iri = getIri(expObj, builder)
      .filter(_.nonEmpty)
      .getOrElse(throw FromJsonException("Ontology without iri is not allowed"))

    graph.ns.getOntology(iri).getOrElse {
      val labels = getTypes(expObj, builder)
      if (labels.size != 1 || labels.head != Ontology.ontology)
        throw FromJsonException(s"Parsing ontology without @type: @class or ${types.rdfsClass}")

      val ontologyNode = MemGraphDefault.ns.nodes.upsert(iri)
      ontologyNode.addLabel(Ontology.ontology)

      getIris(expObj, builder).foreach(iri => ontologyNode --- Property.default.`@ids` --> iri)
      getLabel(expObj, builder).foreach {
        case (lang, label) =>
          ontologyNode --- Property.default.`@label` --> label --- Property.default.`@language` --> lang
      }
      getComment(expObj, builder).foreach {
        case (lang, comment) =>
          ontologyNode --- Property.default.`@comment` --> comment --- Property.default.`@language` --> lang
      }

      expObj
        .get(types.`@extends`)
        .orElse(expObj.get(types.rdfsSubClassOf))
        .map(
          json =>
            json.array
              .map(_.map(_.string
                .map(builder.expandIri(_))
                .getOrElse(throw FromJsonException("jsonToOntology: unknown @type key/iri format"))))
              .orElse(json.string.map(builder.expandIri(_)).map(List(_)))
              .getOrElse(List())
              .map { iri =>
                graph.ns
                  .getOntology(iri)
                  .orElse(httpClient
                    .getResource(iri)({ string =>
                      val json = Parse.parse(string).right.getOrElse(throw FromJsonException(""))
                      json.obj
                        .map(jsonToOntology(_) match {
                          case Success(r) => r
                          case Failure(e) => throw e
                        })
                        .getOrElse(throw FromJsonException(s"Error while processing term $iri expanded iri $iri"))
                    })
                    .toOption)
                  .map {
                    case ontology: Ontology => ontology
                    case _                  => throw FromJsonException(s"@extends $iri does not link to an ontology")
                  }
                  .getOrElse(throw new Exception(s"Could not get extended ontology $iri")) /*.map(DetachedGraph.nodes.upsert)*/
                MemGraphDefault.ns.nodes.hasIri(iri).headOption.getOrElse(graph.ns.nodes.hasIri(iri).head)
            })
        .getOrElse(List())
        .foreach { extendedNode =>
          ontologyNode --- Property.default.`@extends` --> extendedNode
        }

      getProperties(expObj, builder).foreach { propertyNode =>
        ontologyNode --- Property.default.`@properties` --> propertyNode
      }

      (JsonObject.fromTraversableOnce(expObj) - types.`@id` - types.`@ids` - types.`@type` -
        types.`@extends` - types.rdfsSubClassOf - types.`@label` - types.rdfsLabel -
        types.`@comment` - types.rdfsComment).toMap.foreach {
        case (jKey, jValue) => jsonToEdge(ontologyNode, jKey, jValue, builder)
      }

      Ontology(ontologyNode)
    }
  }

  private def getClassType(iri: String): Option[ClassType[_]] = {
    graph.ns
      .getClassType(iri)
      .orElse(
        httpClient
          .getResource(iri)({ string =>
            val json = Parse.parse(string).right.getOrElse(throw FromJsonException(""))
            json.obj
              .flatMap(jsonToClassType(_).toOption)
              .getOrElse(throw FromJsonException(s"Error while processing term $iri expanded iri $iri"))
          })
          .toOption)
      .map {
        case ct: ClassType[_] => ct
        case _                => throw FromJsonException(s"@range $iri does not link to a class-type")
      }
  }

  def jsonToProperty(_obj: JsonObject, _builder: LDGraphBuilder = LDGraphBuilder()): Try[Property] =
    Try {
      val (builder, obj) = parseContext(_obj, _builder).get
      val expObj         = obj.toMap.map { case (key, value) => builder.expandIri(key) -> value }

      val iri = getIri(expObj, builder)
        .filter(_.nonEmpty)
        .getOrElse(throw FromJsonException("Property without iri is not allowed"))

      graph.ns.getProperty(iri).getOrElse {
        val labels = getTypes(expObj, builder)
        if (labels.size != 1 || labels.head != Property.ontology)
          throw FromJsonException(s"Parsing property without @type: @property or ${types.rdfProperty}")

        val propertyNode = MemGraphDefault.ns.nodes.upsert(iri)
        propertyNode.addLabel(Property.ontology)

        getIris(expObj, builder).foreach(iri => propertyNode --- Property.default.`@ids` --> iri)
        getLabel(expObj, builder).foreach {
          case (lang, label) =>
            propertyNode --- Property.default.`@label` --> label --- Property.default.`@language` --> lang
        }
        getComment(expObj, builder).foreach {
          case (lang, comment) =>
            propertyNode --- Property.default.`@comment` --> comment --- Property.default.`@language` --> lang
        }

        expObj
          .get(types.`@extends`)
          .orElse(expObj.get(types.rdfsSubPropertyOf))
          .map(
            json =>
              json.array
                .map(_.map(_.string
                  .map(builder.expandIri(_))
                  .getOrElse(throw FromJsonException("jsonToProperty/@extends: unknown @type key/iri format"))))
                .orElse(json.string.map(builder.expandIri(_)).map(List(_)))
                .getOrElse(List())
                .map { iri =>
                  graph.ns
                    .getProperty(iri)
                    .orElse(httpClient
                      .getResource(iri)({ string =>
                        val json = Parse.parse(string).right.getOrElse(throw FromJsonException(""))
                        json.obj
                          .map(jsonToProperty(_) match {
                            case Success(r) => r
                            case Failure(e) => throw e
                          })
                          .getOrElse(throw FromJsonException(s"Error while processing term $iri expanded iri $iri"))
                      })
                      .toOption)
                    .map {
                      case property: Property => property
                      case _ =>
                        throw FromJsonException(s"@extends $iri does not link to a property")
                    }
                    .getOrElse(throw new Exception(s"Could not get extended property $iri")) /*.map(DetachedGraph.nodes.upsert)*/
                  MemGraphDefault.ns.nodes.hasIri(iri).headOption.getOrElse(graph.ns.nodes.hasIri(iri).head)
              })
          .getOrElse(List())
          .foreach { extendedNode =>
            propertyNode --- Property.default.`@extends` --> extendedNode
          }

        getProperties(expObj, builder).foreach { propertyNode0 =>
          propertyNode --- Property.default.`@properties` --> propertyNode0
        }
        expObj
          .get(types.`@range`)
          .orElse(expObj.get(types.schemaRange))
          .map { json =>
            json.array
              .map(
                _.map(
                  json =>
                    json.string
                      .map(builder.expandIri(_))
                      .flatMap(getClassType)
                      .orElse(json.obj.map(jsonToClassType(_, builder) match {
                        case Success(r) => r
                        case Failure(e) => throw e
                      }))
                      .getOrElse(throw FromJsonException("jsonToProperty/@ranges: unknown @type key/iri format"))))
              .orElse(json.string.map(builder.expandIri(_)).map(List(_).map(getClassType(_).get)))
              .orElse(json.obj.map(jsonToClassType(_, builder) match {
                case Success(r) => List(r)
                case Failure(e) => throw e
              }))
              .getOrElse(List())
//              .map(ct =>
//                MemGraphDefault.ns.nodes
//                  .hasIri(ct.iri)
//                  .headOption
//                  .getOrElse(throw FromJsonException("jsonToProperty/@range: unknown @type key/iri format")))
          }
          .getOrElse(List())
          .foreach { rangeNode =>
            propertyNode --- Property.default.`@range` --> rangeNode
          }

        (JsonObject.fromTraversableOnce(expObj) - types.`@id` - types.`@ids` - types.`@type` -
          types.`@extends` - types.rdfsSubPropertyOf - types.`@label` - types.rdfsLabel -
          types.`@comment` - types.rdfsComment - types.`@container` - types.`@range` -
          types.schemaRange - types.schemaDomainIncludes).toMap.foreach {
          case (jKey, jValue) => jsonToEdge(propertyNode, jKey, jValue, builder)
        }

        Property(propertyNode)
      }
    }

  def jsonToDataType(_obj: JsonObject, _builder: LDGraphBuilder = LDGraphBuilder()): Try[DataType[_]] = Try {
    val (builder, obj) = parseContext(_obj, _builder).get
    val expObj         = obj.toMap.map { case (key, value) => builder.expandIri(key) -> value }

    val iri = getIri(expObj, builder)
      .filter(_.nonEmpty)
      .getOrElse(throw FromJsonException("DataType without iri is not allowed"))

    graph.ns.getDataType(iri).getOrElse {

      val labels = getTypes(expObj, builder)
      if (labels.size != 1 || labels.head != DataType.ontology)
        throw FromJsonException(s"DataType datatype without @type: @datatype or ${types.schemaDataType}")

      val datatypeNode = MemGraphDefault.ns.nodes.upsert(iri)
      datatypeNode.addLabel(DataType.ontology)
      datatypeNode.addLabel(Ontology.ontology)

      expObj
        .get(types.`@extends`)
        .orElse(expObj.get(types.rdfsSubClassOf))
        .map(
          json =>
            json.array
              .map(_.map(_.string
                .map(builder.expandIri(_))
                .getOrElse(throw FromJsonException("jsonToDataType/@extends: unknown @type key/iri format"))))
              .orElse(json.string.map(builder.expandIri(_)).map(List(_)))
              .getOrElse(List())
              .map { iri =>
                graph.ns
                  .getDataType(iri)
                  .orElse(httpClient
                    .getResource(iri)({ string =>
                      val json = Parse.parse(string).right.getOrElse(throw FromJsonException(""))
                      json.obj
                        .map(jsonToDataType(_) match {
                          case Success(r) => r
                          case Failure(e) => throw e
                        })
                        .getOrElse(throw FromJsonException(s"Error while processing term $iri expanded iri $iri"))
                    })
                    .toOption)
                  .map {
                    case datatype: DataType[_] => datatype
                    case _                     => throw FromJsonException(s"@extends $iri does not link to a datatype")
                  }
                  .getOrElse(throw new Exception(s"Could not get extended datatype $iri")) /*.map(DetachedGraph.nodes.upsert)*/
                MemGraphDefault.ns.nodes.hasIri(iri).headOption.getOrElse(graph.ns.nodes.hasIri(iri).head)
            })
        .getOrElse(List())
        .foreach { extendedNode =>
          datatypeNode --- Property.default.`@extends` --> extendedNode
        }

      (JsonObject.fromTraversableOnce(expObj) - types.`@id` - types.`@ids` - types.`@type` -
        types.`@extends` - types.rdfsSubPropertyOf - types.`@label` - types.rdfsLabel -
        types.`@comment` - types.rdfsComment - types.`@container` - types.`@range` -
        types.schemaRange - types.schemaDomainIncludes - CollectionType.keys.valueRange.iri - MapType.keys.keyRange.iri).toMap
        .foreach {
          case (jKey, jValue) => jsonToEdge(datatypeNode, jKey, jValue, builder)
        }

      def jsonToClassTypes(json: Json) = {
        json.string
          .map(
            iri =>
              graph.ns
                .getClassType(iri)
                .get)
          .orElse(json.obj.map(jsonToDataType(_, builder).get))
          .map(List(_))
          .orElse(
            json.array.map(
              _.flatMap(
                json =>
                  json.string
                    .map(iri =>
                      graph.ns
                        .getClassType(iri)
                        .get)
                    .orElse(json.obj.map(jsonToDataType(_, builder).get)))))
      }
      def keyRangeToNode(node: Node)(ct: ClassType[_]) = ct match {
        case ct: DataType[_] => node --- MapType.keys.keyRange --> ct
        case ct: Property    => node --- MapType.keys.keyRange --> ct
        case ct: Ontology    => node --- MapType.keys.keyRange --> ct
      }
      def valueRangeToNode(node: Node)(ct: ClassType[_]) = ct match {
        case ct: DataType[_] => node --- CollectionType.keys.valueRange --> ct
        case ct: Property    => node --- CollectionType.keys.valueRange --> ct
        case ct: Ontology    => node --- CollectionType.keys.valueRange --> ct
      }
      val dt = iri match {
        case iri if iri.startsWith(types.`@list`) =>
          val clsTypes = expObj
            .get(CollectionType.keys.valueRange.iri)
            .flatMap(jsonToClassTypes)
            .getOrElse(List())
          clsTypes.foreach(valueRangeToNode(datatypeNode))
          datatypeNode --- Property.default.`@id` --> ListType(clsTypes).iri
          ListType.wrap(datatypeNode)
        case iri if iri.startsWith(types.`@listset`) =>
          val clsTypes = expObj
            .get(CollectionType.keys.valueRange.iri)
            .flatMap(jsonToClassTypes)
            .getOrElse(List())
          clsTypes.foreach(valueRangeToNode(datatypeNode))
          datatypeNode --- Property.default.`@id` --> ListSetType(clsTypes).iri
          ListSetType.wrap(datatypeNode)
        case iri if iri.startsWith(types.`@set`) =>
          val clsTypes = expObj
            .get(CollectionType.keys.valueRange.iri)
            .flatMap(jsonToClassTypes)
            .getOrElse(List())
          clsTypes.foreach(valueRangeToNode(datatypeNode))
          datatypeNode --- Property.default.`@id` --> SetType(clsTypes).iri
          SetType.wrap(datatypeNode)
        case iri if iri.startsWith(types.`@vector`) =>
          val clsTypes = expObj
            .get(CollectionType.keys.valueRange.iri)
            .flatMap(jsonToClassTypes)
            .getOrElse(List())
          clsTypes.foreach(valueRangeToNode(datatypeNode))
          datatypeNode --- Property.default.`@id` --> VectorType(clsTypes).iri
          VectorType.wrap(datatypeNode)
        case iri if iri.startsWith(types.`@map`) =>
          val kclsTypes =
            expObj.get(MapType.keys.keyRange.iri).flatMap(jsonToClassTypes).getOrElse(List())
          val clsTypes = expObj
            .get(CollectionType.keys.valueRange.iri)
            .flatMap(jsonToClassTypes)
            .getOrElse(List())
          kclsTypes.foreach(keyRangeToNode(datatypeNode))
          clsTypes.foreach(valueRangeToNode(datatypeNode))
          datatypeNode --- Property.default.`@id` --> MapType(kclsTypes, clsTypes).iri
          MapType.wrap(datatypeNode)
      }
      MemGraphDefault.ns.storeClassType(dt.asInstanceOf[ClassType[Any]])
      dt
    }
  }

  def jsonToClassType(obj: JsonObject, builder: LDGraphBuilder = LDGraphBuilder()): Try[ClassType[_]] =
    Try {
      val iri    = getIri(obj.toMap, builder)
      val labels = getTypes(obj.toMap, builder)
      Try { iri.flatMap(graph.ns.getClassType).get } orElse {
        if (labels.contains(DataType.ontology)) {
          jsonToDataType(obj, builder)
        } else if (labels.contains(Property.ontology)) {
          jsonToProperty(obj, builder)
        } else if (labels.contains(Ontology.ontology)) {
          jsonToOntology(obj, builder)
        } else Try { iri.flatMap(getClassType).getOrElse(throw FromJsonException("not valid classtype")) }
      }
    }.flatten

  private def getProperties(obj: Map[String, Json], builder: LDGraphBuilder): List[Node] = {
    obj
      .get(types.`@properties`)
//      .orElse(obj.get(types.schemaDomainIncludes))
      .map(
        json =>
          json.array
            .map(_.map(_.string
              .map(builder.expandIri(_))
              .getOrElse(throw FromJsonException("getProperties: unknown @type key/iri format"))))
            .orElse(json.string.map(builder.expandIri(_)).map(List(_)))
            .getOrElse(List())
            .map { iri =>
              graph.ns
                .getProperty(iri)
                .orElse(httpClient
                  .getResource(iri)({ string =>
                    val json = Parse.parse(string).right.getOrElse(throw FromJsonException(""))
                    json.obj
                      .flatMap(jsonToProperty(_).toOption)
                      .getOrElse(throw FromJsonException(s"Error while processing term $iri expanded iri $iri"))
                  })
                  .toOption)
                .map {
                  case property: Property => property
                  case _                  => throw FromJsonException(s"@properties $iri does not link to a property")
                }
                .getOrElse(throw new Exception("123")) /*.map(DetachedGraph.nodes.upsert)*/
              MemGraphDefault.ns.nodes.hasIri(iri).headOption.getOrElse(graph.ns.nodes.hasIri(iri).head)
          })
      .getOrElse(List())
  }

  def getClassType[T](obj: JsonObject,
                      classType: ClassType[T],
                      builder: LDGraphBuilder = LDGraphBuilder()): Try[(List[ClassType[_]], JsonObject)] = Try {
    obj(types.`@type`)
      .map { json =>
        json.array
          .map { values =>
            values.map { value =>
              value.string.getOrElse(
                throw FromJsonException(s"@type-value can only be a string and not ${value.getClass}"))
            }
          }
          .orElse(json.string.map(List(_)))
          .getOrElse(throw FromJsonException(s"@type-value can only be a string or array of strings"))
      }
      .map { tpes =>
        if (tpes.isEmpty /*lengthCompare(1) == 0*/ ) throw FromJsonException("empty @type")
        //        val iri = builder.expandIri(tpes.head)
        tpes
          .map(builder.expandIri(_))
          .map { iri =>
            if (iri == types.`@id`) DataType.default.`@url`
            else //key.range.collectFirst { case key if key.iri == iri => key }
              //          .orElse {
              graph.ns
                .getClassType(iri)
                .getOrElse {
                  throw FromJsonException(s"no classtype found for iri ${iri}")
                }
          }
          .asInstanceOf[List[ClassType[_]]] -> (obj - types.`@type`)
      //        println(s"classtype == ${iri}")
      }
      .getOrElse {
        List(classType) -> obj
      }
  }

  def jsonToEdge[T <: Resource[_]](resource: T,
                                   jKey: String,
                                   jValue: Json,
                                   builder: LDGraphBuilder = LDGraphBuilder()): List[Edge[_, _]] = {
    val iri = builder.expandIri(jKey)
    val (key, classType) = {
      val key = //builder.properties.get(iri).orElse(
        graph.ns.getProperty(iri).getOrElse {
          httpClient.getResource(iri) { json =>
            this
              .parseContext(Parse.parseOption(json).flatMap(_.obj).getOrElse(throw FromJsonException("")))
              .flatMap {
                case (builder, obj) => this.jsonToProperty(obj, builder)
              } match {
              case Success(key) => key
              case Failure(e) =>
                println(e.getMessage)
                e.printStackTrace()
                throw FromJsonException(s"Error while processing term $jKey expanded iri $iri")
            }
          } match {
            case Success(key) => key
            case Failure(e) =>
              println(e.getMessage)
              //              throw FromJsonException(s"Unknown term $jKey expanded iri $iri")
              val key = Property(iri)
              MemGraphDefault.ns.storeProperty(key)
              key
          }
        }
      key -> builder.context.property
        .get(key)
        .flatMap(_.tpe)
        .getOrElse(key.range.headOption.getOrElse(DataType.default.`@string`))
    }

    key.container match {
      case Some(types.`@list`) =>
        jValue.array
          .map { values =>
            values.map(json =>
              jsonToValue(classType, json, builder) match {
                case Success((classType, value)) =>
                  resource
                    .addOut(TypedProperty(key, classType), value) //.asInstanceOf[Property[S, T]]
                case Failure(error) =>
                  //              error.printStackTrace()
                  //              throw FromJsonException(s"Could not parse value $json for ${key.iri} with dataType ${classType.iri} and error ${error.getMessage}")
                  throw error
            })
          }
          //          .orElse{jValue.obj.map{obj =>
          //          val expObj = obj.toMap.map { case (key, value) => builder.expandIri(key) -> value }
          //          getTypes(expObj, builder) match {
          //          case Success((classType, value)) => resource.addOut(TypedPropertyKey(key, classType), value) //.asInstanceOf[Property[S, T]]
          //          case Failure(error) => throw FromJsonException(s"Could not parse value $json for ${key.iri} with dataType ${classType.iri} and error ${error.getMessage}")
          //        }}}
          .getOrElse(throw FromJsonException(s"@container:@list but value $jValue is not a @list"))
      case Some(types.`@set`) =>
        jValue.array
          .map { values =>
            values.map(json =>
              jsonToValue(classType, json, builder) match {
                case Success((classType, value)) =>
                  resource
                    .addOut(TypedProperty(key, classType), value) //.asInstanceOf[Property[S, T]]
                case Failure(error) =>
                  throw FromJsonException(
                    s"Could not parse value $json for ${key.iri} with dataType ${classType.iri} and error ${error.getMessage}")
            })
          }
          .getOrElse(throw FromJsonException(s"@container:@set but value $jValue for key ${key.iri} is not a @set"))
      case Some(types.`@language`) | Some(types.`@index`) =>
        jValue.obj
          .map { obj =>
            jsIndexToEdges(resource, key, key.container.get, classType, obj, builder) match {
              case Success(value) => value
              case Failure(error) => throw error
            }
          }
          .getOrElse(throw FromJsonException(s"@container:@language/@index but value is not a ${key.container.get}"))
      case _ =>
        jsonToValue(classType, jValue, builder) match {
          case Success((classType, value)) =>
            List(resource.addOut(key + classType, value)) //.asInstanceOf[Property[S, T]]
          case Failure(error) =>
            throw FromJsonException(
              s"Could not parse value $jValue for ${key.iri} with dataType ${classType.iri} and error ${error.getMessage}")
        }
    }
  }

  def jsValueToValue(obj: JsonObject,
                     classType: ClassType[_],
                     builder: LDGraphBuilder = LDGraphBuilder()): Try[(ClassType[Any], Any)] = Try {
    parseContext(obj, builder).map {
      case (builder, obj) =>
        obj(types.`@value`) match {
          case Some(value) =>
            getClassType(obj, classType, builder) match {
              case Success((classTypes, obj)) =>
                jsonToValue(classTypes.head, value, builder) match {
                  case Success((dt, value)) =>
                    dt -> value
                  case Failure(error) =>
                    throw FromJsonException(
                      s"Could not parse @value $value for ${classType.iri} because ${error.getMessage}")
                }
              case Failure(error) =>
                throw FromJsonException(s"Could not parse @type because ${error.getMessage}")
            }
          case None =>
            throw FromJsonException(s"@value expected but missing for ${classType.iri} in $obj")
        }
    } match {
      case Success(value) => value //.toList//.asInstanceOf[List[MemProperty[_, _]]]
      case Failure(error) => throw error
    }
  }

  def jsToToEdge(obj: JsonObject,
                 property: Property,
                 builder: LDGraphBuilder = LDGraphBuilder()): Try[(ClassType[Any], Any)] = Try {
    parseContext(obj, builder).map {
      case (builder, obj) =>
        val to = obj(types.`@to`) match {
          case Some(to) =>
            val classType = property.range.headOption.getOrElse(TextType.textType)
            getClassType(obj, classType, builder) match {
              case Success((classTypes, obj)) =>
                jsonToValue(classTypes.head, to, builder) match {
                  case Success((dt, value)) =>
                    dt -> value
                  case Failure(error) =>
                    throw FromJsonException(s"Could not parse @to $to for ${classType.iri} because ${error.getMessage}")
                }
              case Failure(error) =>
                throw FromJsonException(s"Could not parse @type because ${error.getMessage}")
            }
          case None =>
            throw FromJsonException(s"@to expected but missing for ${property.iri} in $obj")
        }
        val from = obj(types.`@from`) match {
          case Some(from) =>
            val classType = property.range.headOption.getOrElse(TextType.textType)
            getClassType(obj, classType, builder) match {
              case Success((classTypes, obj)) =>
                jsonToValue(classTypes.head, from, builder) match {
                  case Success((dt, value)) =>
                    dt -> value
                  case Failure(error) =>
                    throw FromJsonException(
                      s"Could not parse @from $from for ${classType.iri} because ${error.getMessage}")
                }
              case Failure(error) =>
                throw FromJsonException(s"Could not parse @type because ${error.getMessage}")
            }
          case None =>
            throw FromJsonException(s"@from expected but missing for ${property.iri} in $obj")
        }
        val fromResource = from._2 match {
          case o: Node       => o
          case p: Edge[_, _] => p
          case v: Value[_]   => v
          case v             => DetachedGraph.values.create(v, from._1.asInstanceOf[DataType[Any]])
        }
        val toResource = from._2 match {
          case o: Node       => o
          case p: Edge[_, _] => p
          case v: Value[_]   => v
          case v             => DetachedGraph.values.create(v, from._1.asInstanceOf[DataType[Any]])
        }
        property -> DetachedGraph.edges.create(fromResource, property, toResource)
    } match {
      case Success(value) => value //.toList//.asInstanceOf[List[MemProperty[_, _]]]
      case Failure(error) => throw error
    }
  }

  def jsIndexToEdges[T <: Resource[_]](resource: T,
                                       key: Property,
                                       container: String,
                                       classType: ClassType[_],
                                       obj: JsonObject,
                                       builder: LDGraphBuilder = LDGraphBuilder()): Try[List[Edge[_, _]]] = Try {
    if (obj.fieldSet.intersect(Graph.reservedKeys.map(_.iri)).nonEmpty)
      throw FromJsonException("index-keys cannot contain reserved key-words")
    container match {
      case types.`@language` =>
        obj.toMap.map {
          case (indexKey, jValue) =>
            //                jsonToValue(typedKey.self2, key, context)
            //                  val newProperty = resource.property(, indexKey).head //asInstanceOf[List[Property[S, Any]]]
            jsonToValue(classType, jValue, builder) match {
              case Success((dt, value)) =>
                val property = resource.addOut(key, dt, value)
                property.addOut(Property.default.typed.languageString, indexKey)
                property
              case Failure(error) => throw error
            }
        }.toList //.asInstanceOf[List[Property[_, String]]]
      case types.`@index` =>
        obj.toMap.map {
          case (indexKey, jValue) =>
            //                jsonToValue(typedKey.self2, key, context)
            //                  val newProperty = resource.property(key, indexKey).head //asInstanceOf[List[Property[S, Any]]]
            jsonToValue(classType, jValue, builder) match {
              case Success((dt, value)) =>
                val property = resource.addOut(key, dt, value)
                property.addOut(Property.default.typed.indexString, indexKey)
                property
              case Failure(error) => throw error
            }
        }.toList //.asInstanceOf[List[Property[_, String]]]
      //            case ldcontext.types.
    }
  }

  def jsonToValue[T](classType: ClassType[T],
                     json: Json,
                     builder: LDGraphBuilder = LDGraphBuilder()): Try[(ClassType[T], T)] = Try {

    json.obj
      .flatMap { obj =>
        obj(types.`@value`)
          .map(json =>
            jsValueToValue(obj, classType, builder) match {
              case Success(r)     => r
              case Failure(error) => throw error
          })
          .orElse {
            Option(if (obj ?? types.`@to` && obj ?? types.`@from` && obj ?? types.`@type`) {
              getClassType(obj, classType, builder) match {
                case Success(r) =>
                  if (r._1.headOption.exists(_.isInstanceOf[Property])) {
                    jsToToEdge(r._2, r._1.head.asInstanceOf[Property], builder) match {
                      case Success(r)     => r
                      case Failure(error) => throw error
                    }
                  } else if (r._1.headOption.contains(IriType)) {
                    jsToToEdge(r._2, Property.default.`@id`, builder) match {
                      case Success(r)     => r
                      case Failure(error) => throw error
                    }
                  } else null
                case Failure(e) => throw e
              }
            } else null)
          }
      }
      .orElse {
        (classType match {
          case tpe: LiteralType[_] =>
            val value = tpe match {
              case TextType.textType =>
                json.string.map(tpe -> _)
              case tpe: NumericType[_] =>
                (tpe match {
                  case DoubleType.doubleType => json.number.flatMap(_.toDouble)
                  case LongType.longType     => json.string.map(_.toLong)
                  case IntType.intType       => json.number.flatMap(_.toInt)
                }).map(tpe -> _)
              case tpe: CalendarType[_] =>
                (tpe match {
                  case DateTimeType.datetimeType => json.string.map(Instant.parse(_))
                  case LocalDateType.default     => json.string.map(LocalDate.parse(_))
                  case LocalTimeType.default     => json.string.map(LocalTime.parse(_))
                }).map(tpe -> _)
              //        case tpe: ColorType[_] =>
              case BoolType.boolType => json.bool.map(tpe -> _)
            }
            value.orElse(jsonToValue(DataType.default.`@url`, json, builder).toOption) //.getOrElse(throw FromJsonException(s"@type ${classType.iri} not corresponding with data-structure")))
          case tpe: StructuredValue[_] =>
            tpe match {
              case tpe: GeometricType[_] =>
                fromGeoJson(json)
                  .map(tpe -> _) //.getOrElse(throw FromJsonException("not valid geojson"))
              case tpe: CollectionType[_] =>
                (tpe match {
                  case tpe: ListType[_] =>
                    json.array
                      .map(
                        array =>
                          array
                            .map(json =>
                              jsonToValue(tpe.valueRange.headOption.getOrElse(DataType.default.`@string`),
                                          json,
                                          builder) match {
                                case Success(value) => value._2
                                case Failure(error) => throw error
                            }))
                  case tpe: MapType[_, _] =>
                    json.array.map { array =>
                      array
                        .flatMap(json =>
                          json.array.filter(_.lengthCompare(2) == 0).map { array =>
                            (jsonToValue(tpe.keyRange.headOption.getOrElse(DataType.default.`@string`),
                                         array.head,
                                         builder) match {
                              case Success(value) => value._2
                              case Failure(error) => throw error
                            }) -> (jsonToValue(tpe.valueRange.headOption.getOrElse(DataType.default.`@string`),
                                               array(1),
                                               builder) match {
                              case Success(value) => value._2
                              case Failure(error) => throw error
                            })
                        })
                        .toMap
                    }
                  case tpe: ListSetType[_] =>
                    json.array
                      .map(
                        array =>
                          array
                            .map(json =>
                              jsonToValue(tpe.valueRange.headOption.getOrElse(DataType.default.`@string`),
                                          json,
                                          builder) match {
                                case Success(value) => value._2
                                case Failure(error) => throw error
                            }))
                  case tpe: SetType[_] =>
                    json.array
                      .map(
                        array =>
                          array
                            .map(json =>
                              jsonToValue(tpe.valueRange.headOption.getOrElse(DataType.default.`@string`),
                                          json,
                                          builder) match {
                                case Success(value) => value._2
                                case Failure(error) => throw error
                            }))
                  case tpe: VectorType[_] =>
                    json.array
                      .map(
                        array =>
                          array
                            .map(json =>
                              jsonToValue(tpe.valueRange.headOption.getOrElse(DataType.default.`@string`),
                                          json,
                                          builder) match {
                                case Success(value) => value._2
                                case Failure(error) => throw error
                            }))
                }).map(tpe -> _)
            }
          //      case tpe: IriType[_] =>
          //        json.string.map { iri =>
          //          //TODO: check for valid iri
          //          IriResource(builder.expandIri(iri)) //.asInstanceOf[T]
          //        }.orElse(json.obj.flatMap(_ (types.id).flatMap(_.string.map(iri => IriResource(builder.expandIri(iri))))))
          //          .getOrElse(throw FromJsonException(s"No valid iri"))
          case _: Ontology | _: IriType[_] =>
            json.string
              .map { iri =>
                //TODO: check for valid iri
                val node = DetachedGraph.nodes.create()
                node.addOut(Property.default.typed.iriUrlString, builder.expandIri(iri))
                DataType.default.`@url` -> node //.asInstanceOf[T]
              }
              .orElse(json.obj
                .map { obj =>
                  resource(obj, builder) match {
                    case Success(resource) =>
                      DataType.default.`@url` -> resource //.asInstanceOf[T]
                    case Failure(error) => throw error
                  }
                }
                .asInstanceOf[Option[(ClassType[Any], Any)]])

          case tpe: Property =>
            throw new Exception("jsonToValue for type @property is not supported")
          case tpe: DataType[_] =>
            throw new Exception("jsonToValue for type generic @datatype is not supported")
        })
      }
      .asInstanceOf[Option[(ClassType[T], T)]]
      .getOrElse(throw FromJsonException(s"${classType.iri} expected, could not parse $json"))
  }

  def resource(obj: JsonObject,
               builder: LDGraphBuilder = LDGraphBuilder(),
               expectedOntologies: ListSet[Ontology] = ListSet[Ontology](),
               sourceUrl: Option[String] = None): Try[Resource[_]] = Try {
    parseContext(obj, builder).map {
      case (builder, obj) =>
        val expObj = obj.toMap.map { case (key, value) => builder.expandIri(key) -> value }

        if (obj.size == 1 && obj ?? types.`@graph` && sourceUrl.exists(_.startsWith("https://schema.org"))) {
          val result = {
            val json = obj(types.`@graph`).get
            json.array
              .map(_.map(json =>
                json.string
                  .map(iri => JsonObject.fromTraversableOnce(Map(types.`@id` -> Json.jString(iri))))
                  .orElse(json.obj)
                  .getOrElse(throw FromJsonException(s"@graph cannot contain nested lists"))).map { obj =>
                getIri(expObj, builder)
                  .getOrElse(throw FromJsonException("invalid ontology reference")) -> obj //(json - ldcontext.types.id)
              })
              .getOrElse(throw FromJsonException(
                s"@graph should contain an JsArray, got ${json.getClass.getSimpleName}"))
          }
          val newBuilder = builder.copy(iris = builder.iris ++ result.map(_._1).toSet)
          result
            .find(
              _._1 == sourceUrl
                .getOrElse(throw new Exception("emptySourceUrl"))
                .stripSuffix(".jsonld"))
            .map {
              case (iri, obj) =>
                resource(obj, newBuilder, sourceUrl = Some(iri))
                  .getOrElse(throw FromJsonException(s"invalid schema.org resource"))
            }
            .getOrElse(throw FromJsonException(s"no schema.org resource"))
        } else {
          val ontologies = getTypes(obj.toMap, builder) //.getOrElse(expectedOntologies.toList)

          val iri  = getIri(expObj, builder)  //.foreach(node.addOut(Property.default.id, _))
          val iris = getIris(expObj, builder) //.foreach(iri => node.addOut(Property.default.ids, iri))

          iri
            .flatMap(
              iri =>
                if (ontologies.exists(_.iri == types.`@datatype`) || ontologies
                      .exists(_.iri == types.`@class`) || ontologies
                      .exists(_.iri == types.`@property`)) {
                  MemGraphDefault.ns.nodes.hasIri(iri).headOption
                } else {
                  None
              })
            .getOrElse {
              val node = DetachedGraph.nodes.create()
              ontologies.foreach(node.addLabel)
              iri.foreach(node --- Property.default.`@id` --> _)
              iris.foreach(node --- Property.default.`@ids` --> _)
              //                propertyValues(node, obj - ldcontext.types.id - ldcontext.types.ids - ldcontext.types.context - ldcontext.types.TYPE, builder)
              (obj - types.`@id` - types.`@ids` - types.`@context` - types.`@type`).toMap.foreach {
                case (jKey, jValue) =>
                  jsonToEdge(node, jKey, jValue, builder)
              }
              node
            }
        }
    } match {
      case Success(result) => result
      case Failure(error)  => throw error
    }
  }
}
