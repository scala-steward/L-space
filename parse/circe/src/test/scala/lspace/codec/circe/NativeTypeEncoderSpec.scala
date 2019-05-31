package lspace.codec.circe

class NativeTypeEncoderSpec extends lspace.codec.jsonld.NativeTypeEncoderSpec {
  type Json = io.circe.Json
  implicit val encoder = lspace.codec.circe.nativeEncoder

  def toMinString(json: Json): String = json.noSpaces
}
