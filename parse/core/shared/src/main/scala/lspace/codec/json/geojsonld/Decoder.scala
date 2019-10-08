package lspace.codec.json.geojsonld

import lspace.Label
import lspace.codec.{`@container`, ActiveContext, ActiveProperty, ExpandedMap}
import lspace.codec.exception.FromJsonException
import lspace.codec.json
import lspace.codec.json.geojson.GeoJsonDecoder
import lspace.datatype.DataType.default._
import lspace.datatype.{CollectionType, ListType}
import lspace.provider.detached.DetachedGraph
import lspace.structure.{ClassType, Edge, Property, Resource}
import lspace.types.geo.{Geometry, MultiGeometry}
import monix.eval.Task

class Decoder[Json](decoder: json.jsonld.JsonLDDecoder[Json], geoJsonDecoder: GeoJsonDecoder[Json])
    extends lspace.codec.Decoder {
  import decoder._
  import decoder.baseDecoder._

  def withJsonProperties(resource: lspace.Node, otherJson: ExpandedMap[Json])(
      implicit activeContext: ActiveContext): Task[lspace.Node] = {
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
            .map {
              obj =>
                activeProperty.`@container` match {
                  case Nil =>
                    toResource(obj.expand, expectedType)
                      .onErrorHandleWith { case t => autodiscoverValue(json) }
                      .flatMap(addEdgeF(_))
                      .map(List(_))
                  case List(`@container`.`@index`) =>
                    Task.gatherUnordered(obj.toList.map { //TODO: expand keys
                      case (index, json) =>
                        toResource(json, expectedType)
                          .onErrorHandleWith { case t => autodiscoverValue(json) }
                          .flatMap(addEdgeF(_))
                          .flatMap(_.addOut(Label.P.`@index`, index))
                    })
                  case List(`@container`.`@language`) =>
                    Task.gatherUnordered(obj.toList.map {
                      case (language, json) =>
                        toResource(json, expectedType)
                          .onErrorHandleWith { case t => autodiscoverValue(json) }
                          .flatMap(addEdgeF(_))
                          .flatMap(_.addOut(Label.P.`@language`, language))
                    })
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
                              .flatMap(addEdgeTypedF(collectionType, _))
                              .map(List(_))
                          case et => //no container but expected type, try ListType(List(et))
                            toCollection(array, ListType(et))
                              .flatMap(nodes => Task.gatherUnordered(nodes.map(node => addEdgeTypedF(et, node))))
                        }
                        .getOrElse {
                          Task.gatherUnordered(array.map(toResource(_, expectedType)
                            .onErrorHandleWith { case t => autodiscoverValue(json) }
                            .flatMap(addEdgeF(_))))
                        }
                    case List(`@container`.`@list`) | List(`@container`.`@set`) =>
                      //this processes an array as a list of edges
                      Task.gatherUnordered(array.map(toResource(_, expectedType)
                        .onErrorHandleWith { case t => autodiscoverValue(json) }
                        .flatMap(addEdgeF(_))))
                  }
                edgesTask
            })
            .getOrElse {
              toResource(json, expectedType)
                .onErrorHandleWith { case e: Throwable => autodiscoverValue(json) }
                .flatMap(addEdgeF(_))
                .map(List(_))
            }
      })
      .flatMap { l =>
        Task.gatherUnordered(l).map { l =>
          resource
        }
      }
  }

  def autodiscoverValue(json: Json): Task[lspace.Value[Any]] = {
    json.int
      .map(graph.values.upsert(_, `@int`))
      .orElse(json.double.map(graph.values.upsert(_, `@double`)))
      .orElse(json.long.map(graph.values.upsert(_, `@long`)))
      .orElse(json.datetime.map(graph.values.upsert(_, `@datetime`)))
      .orElse(json.localdatetime.map(graph.values.upsert(_, `@localdatetime`)))
      .orElse(json.date.map(graph.values.upsert(_, `@date`)))
      .orElse(json.time.map(graph.values.upsert(_, `@time`)))
      .orElse(json.boolean.map(graph.values.upsert(_, `@boolean`)))
      .orElse(decodeGeometryOption(json))
      .orElse(json.string.map(graph.values.upsert(_, `@string`)))
      .getOrElse(Task.raiseError(new Exception("autodiscover not possible")))
  }

  def decodeFeature(map: Map[String, Json])(implicit context: ActiveContext): Task[lspace.Node] = {
    for {
      geometry <- map
        .get("geometry")
        .map(Task.now(_).map(_.obj.map(decodeGeometry).getOrElse(throw new Exception("geometry not an object"))))
        .getOrElse(Task.raiseError(new Exception("no geometry")))
      properties <- map
        .get("properties")
        .map(
          _.obj
            .map(Task.now)
            .getOrElse(Task.raiseError(new Exception("invalid properties"))))
        .getOrElse(Task.now(Map[String, Json]()))
      node <- DetachedGraph.nodes.create()
      //      geo  <- DetachedGraph.values.upsert(geometry, lspace.ClassType.detect(geometry))
      //      _    <- node --- lspace.ns.vocab.schema.location --> geo
      _ <- withJsonProperties(node, properties.expand)
    } yield node
  }
  def decodeFeatureOption(map: Map[String, Json])(implicit context: ActiveContext): Option[Task[lspace.Node]] = {
    if (map.get("type").exists(_.string.contains("Feature"))) Some(decodeFeature(map)) else None
  }
  def decode(json: String)(implicit context: ActiveContext): Task[List[lspace.Node]] = {
    decoder.parse(json).flatMap { json =>
      json.obj
        .map { map =>
          map.get("type").flatMap(_.string) match {
            case Some(iri) =>
              iri match {
                case "FeatureCollection" =>
                  for {
                    features <- map
                      .get("features")
                      .map(_.list
                        .map(Task.now)
                        .getOrElse(Task.raiseError(new Exception("features must be an array"))))
                      .getOrElse(Task.raiseError(new Exception("FeatureCollection without 'features' is weird")))
                    nodes <- Task.gather(
                      features
                        .map(
                          _.obj
                            .map(Task
                              .now(_)
                              .flatMap(
                                decodeFeatureOption(_).getOrElse(Task.raiseError(new Exception("invalid feature")))))
                            .getOrElse(Task.raiseError(new Exception("feature must be an object")))): List[
                        Task[lspace.Node]])
                  } yield nodes
                case "Feature" =>
                  decodeFeature(map).map(List(_))
                case "GeometryCollection" =>
                  for {
                    node <- DetachedGraph.nodes.create()
                    v = geoJsonDecoder.decodeGeometryCollection(map)
                    value <- DetachedGraph.values.upsert(v, lspace.DataType.detect(v))
                    _     <- node --- "http://schema.org/location" --> value
                  } yield List(node)
                case _ =>
                  for {
                    node  <- DetachedGraph.nodes.create()
                    geo   <- decodeGeometryCoordinates(iri, geoJsonDecoder.decodeCoordinates(map))
                    value <- DetachedGraph.values.upsert(geo)
                    _     <- node --- "http://schema.org/location" --> value
                  } yield List(node)
              }
            case None => Task.raiseError(new Exception("invalid geojson"))
          }
        }
        .getOrElse(Task.raiseError(new Exception("invalid geojson")))
    }
  }

  def decodeGeometryOption(json: Json): Option[Task[lspace.Value[Geometry]]] =
    json.obj
      .map(decodeGeometryOption(_).getOrElse(throw new Exception("geometry without type")))
  def decodeGeometry(map: Map[String, Json]): Task[lspace.Value[Geometry]] =
    decodeGeometryOption(map).getOrElse(Task.raiseError(new Exception("geometry without type")))
  def decodeGeometryOption(map: Map[String, Json]): Option[Task[lspace.Value[Geometry]]] = {
    map.get("type").flatMap(_.string).map {
      case "GeometryCollection" => decodeGeometryCollection(map)
      case iri                  => decodeGeometryCoordinates(iri, geoJsonDecoder.decodeCoordinates(map))
    }
  }

  def decodeGeometryCoordinates(iri: String, coordinates: List[Json]): Task[lspace.Value[Geometry]] = {
    iri match {
      case "Point"           => graph.values.upsert(geoJsonDecoder.decodePoint(coordinates), `@geopoint`)
      case "MultiPoint"      => graph.values.upsert(geoJsonDecoder.decodeMultiPoint(coordinates), `@geomultipoint`)
      case "LineString"      => graph.values.upsert(geoJsonDecoder.decodeLine(coordinates), `@geoline`)
      case "MultiLineString" => graph.values.upsert(geoJsonDecoder.decodeMultiLine(coordinates), `@geomultiline`)
      case "Polygon"         => graph.values.upsert(geoJsonDecoder.decodePolygon(coordinates), `@geopolygon`)
      case "MultiPolygon"    => graph.values.upsert(geoJsonDecoder.decodeMultiPolygon(coordinates), `@geomultipolygon`)
      case _                 => Task.raiseError { FromJsonException(s"not a valid geojson Geometry $iri") }
    }
  }

  def decodeGeometryCollection(map: Map[String, Json]): Task[lspace.Value[MultiGeometry]] =
    map.get("geometries") match {
      case Some(json) =>
        Task {
          MultiGeometry(
            json.list
              .map(_.map(_.obj.getOrElse(throw new Exception("invalid MultiGeometry Geometry")))
                .map(geoJsonDecoder.decodeGeometry))
              .getOrElse(throw new Exception("invalid MultiGeometry, array expected for geometries"))
              .toVector)
        }.flatMap(graph.values.upsert(_, `@geomultigeo`))
      case None => Task.raiseError(FromJsonException("not a valid geojson MultiGeometry, geometries missing"))
    }
}
