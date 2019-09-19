package lspace.codec.circe

import lspace.codec.json

class JsonEncoderSpec extends json.JsonEncoderSpec {
  type Json = io.circe.Json
  implicit val encoder = lspace.codec.circe.nativeEncoder

  def toMinString(json: Json): String = json.noSpaces
}
