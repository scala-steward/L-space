package lspace.codec.circe

import io.circe.Json
import lspace.codec._

class NativeTypeEncoderSpec extends lspace.codec.NativeTypeEncoderSpec {
  type Json = io.circe.Json
  implicit val encoder = lspace.codec.circe.nativeEncoder

  def toMinString(json: Json): String = json.noSpaces
}
