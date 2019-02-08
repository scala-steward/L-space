package lspace.lgraph.provider.file

import argonaut._
import Argonaut._
import lspace.NS.types
import lspace.codec.exception.FromJsonException
import lspace.librarian.structure._
import lspace.codec.{ActiveContext, JsonInProgress}
import monix.eval.Task

case class EncodeLDFS(idMaps: IdMaps = IdMaps()) extends lspace.codec.argonaut.Encoder {

  override def fromAny(value: Any, expectedType: Option[ClassType[_]] = None)(implicit activeContext: AC): JIP = {
    value match {
      case resource: IriResource =>
        resource match {
          case value: Value[_] =>
            if (expectedType.contains(value.label)) {
              fromData(value.value, value.label)
            } else {
              val jip = fromData(value.value, value.label)
              JsonInProgress[Json](
                Json.jObject(
                  JsonObject
                    .fromTraversableOnce(
                      Map(types.`@value` -> jip.json, types.`@type` -> value.label.iri.compact.asJson))))(
                jip.activeContext
              )
            }
          case node: Node               => JsonInProgress(node.id.asJson)
          case edge: Edge[_, _]         => JsonInProgress(edge.id.asJson)
          case iriResource: IriResource => JsonInProgress(iriResource.iri.asJson)(activeContext)
        }
      case _ =>
        val label = ClassType.valueToOntologyResource(value)
        if (expectedType.contains(label)) {
          fromData(value, label)
        } else {
          val jip = fromData(value, label)
          JsonInProgress(
            Json.jObject(JsonObject
              .fromTraversableOnce(Map(types.`@value` -> jip.json, types.`@type` -> label.iri.compact.asJson))))(
            jip.activeContext)
        }
    }
  }
}
