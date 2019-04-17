package lspace.codec

import lspace.NS.types

import scala.collection.immutable.ListMap

case class JsonObjectInProgress[Json](json: List[(String, Json)])(implicit val activeContext: ActiveContext) {}

object JsonObjectInProgress {}
