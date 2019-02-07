package lspace.codec.argonaut

import argonaut._
import Argonaut._
import lspace.NS.types
import lspace.codec.{JsonInProgress, JsonObjectInProgress}
import lspace.librarian.datatype.DataType
import lspace.librarian.structure._
import lspace.parse.{ActiveContext, ActiveProperty}
import lspace.types.vector.Geometry

object Encode extends Encode {}
trait Encode extends lspace.codec.Encode[Json, JsonObject] {
  override def getNewActiveContext: AC = ActiveContext()

  override def getNewActiveProperty: AP = ActiveProperty()

  implicit override def jsonObjectToJson(json: JsonObject): Json = Json.jObject(json)

  implicit override def textToJson(text: String): Json = Json.jString(text)

  implicit override def boolToJson(boolean: Boolean): Json = Json.jBool(boolean)

  implicit override def intToJson(int: Int): Json = Json.jNumber(int)

  implicit override def doubleToJson(double: Double): Json = Json.jNumber(double)

  implicit override def longToJson(long: Long): Json = Json.jNumber(long)

  implicit override def geoToJson(geo: Geometry): Json = lspace.encode.GeometryCodecJson(geo)

  implicit override def mapToJson(map: Map[String, Json]): Json = map.asJson

  implicit override def mapToJsonObject(map: Map[String, Json]): JsonObject = JsonObject.fromTraversableOnce(map)

  implicit override def listToJson(list: List[Json]): Json = list.asJson

  implicit override def tuple2ToJson(tuples: (Json, Json)): Json = tuples.asJson

  implicit override def tuple3ToJson(tuples: (Json, Json, Json)): Json = tuples.asJson

  implicit override def tuple4ToJson(tuples: (Json, Json, Json, Json)): Json = tuples.asJson

  implicit override def tuple2ListToJson(tuples: List[(Json, Json)]): Json = tuples.map(_.asJson).asJson

  import lspace.parse.JsonObjectInProgress._
  private val printer = PrettyParams.nospace.copy(preserveOrder = true)
  override def apply[T <: Node](node: Node): String =
    printer.pretty(Json.jObject(fromNode(node)(ActiveContext().asInstanceOf[AC]).withContext))

  def fromNode(node: Node)(implicit activeContext: AC): JOIP = {
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
            JsonObjectInProgress[Json, JsonObject](
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
              }))(
              activeContext
            ).addEdges(nodeResource)
        }
    }
  }

  implicit class WithDictionary(jsonIP: JOIP) {
    implicit val activeContext = jsonIP.activeContext
    def addEdges(resource: Resource[_]): JOIP = {
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
          JsonObjectInProgress[Json, JsonObject](JsonObject.fromTraversableOnce(jsonIP.json.toList ++ result))(
            activeContext)
      }
    }
  }

  def fromEdges(key: Property, edges: List[Edge[_, _]])(implicit activeContext: AC): JIP = {

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
      case null | List() => JsonInProgress[Json, JsonObject](Json.jNull)
      case List(property) /*if key.container.isEmpty*/ =>
        edgeInVToJson(property)(activeContext)
      case edges =>
        edges.foldLeft(List[Json]() -> activeContext) {
          case ((result, activeContext), edge) =>
            val jip = edgeInVToJson(edge)(activeContext)
            (result :+ jip.json) -> jip.activeContext
        } match {
          case (result, activeContext) => JsonInProgress[Json, JsonObject](result.asJson)(activeContext)
        }
    }
  }

  def fromEdge(edge: Edge[_, _])(implicit activeContext: AC): JOIP = {
    val (keyIri, newActiveContext) = activeContext.compactIri(edge.key)
    JsonObjectInProgress[Json, JsonObject](
      JsonObject.fromTraversableOnce(Seq(edge.iri).filter(_.nonEmpty).map(types.`@id` -> Json.jString(_)) ++ {
        val iris = edge.iris
        if (iris.toList.lengthCompare(1) == 0) Seq(types.`@ids` -> Json.jString(iris.head))
        else if (iris.nonEmpty)
          Seq(types.`@ids` -> edge.iris.toList.map(Json.jString).asJson)
        else Seq()
      } ++ Seq(types.`@type` -> Json.jString(keyIri))))(
      activeContext
    ).addEdges(edge)
  }
}
