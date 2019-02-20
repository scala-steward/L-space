package lspace.lgraph.provider.file

import lspace.NS.types
import lspace.structure._
import lspace.codec.{JsonInProgress, NativeTypeEncoder}

case class EncodeLDFS[Json0](idMaps: IdMaps = IdMaps())(implicit val baseEncoder: NativeTypeEncoder.Aux[Json0])
    extends lspace.codec.Encoder {
  type Json = Json0

  override def fromAny(value: Any, expectedType: Option[ClassType[_]] = None)(implicit activeContext: AC): JIP = {
    value match {
      case resource: IriResource =>
        resource match {
          case value: Value[_] =>
            if (expectedType.contains(value.label)) {
              fromData(value.value, value.label)
            } else {
              val jip = fromData(value.value, value.label)
              JsonInProgress(Map(types.`@value` -> jip.json, types.`@type` -> value.label.iri.compact.asJson).asJson)(
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
          JsonInProgress(Map(types.`@value` -> jip.json, types.`@type` -> label.iri.compact.asJson).asJson)(
            jip.activeContext)
        }
    }
  }
}
