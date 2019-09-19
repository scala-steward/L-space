package lspace.codec.argonaut

import lspace.codec.json

class JsonEncoderSpec extends json.JsonEncoderSpec {
  type Json = _root_.argonaut.Json
  implicit val encoder = lspace.codec.argonaut.nativeEncoder

  def toMinString(json: Json): String = json.nospaces
}
