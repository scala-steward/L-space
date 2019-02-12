package lspace.codec.argonaut

class NativeTypeEncoderSpec extends lspace.codec.NativeTypeEncoderSpec {
  type Json = _root_.argonaut.Json
  implicit val encoder = lspace.codec.argonaut.nativeEncoder

  def toMinString(json: Json): String = json.nospaces
}
