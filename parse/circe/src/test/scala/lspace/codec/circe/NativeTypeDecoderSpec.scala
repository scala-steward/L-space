package lspace.codec.circe

class NativeTypeDecoderSpec extends lspace.codec.jsonld.NativeTypeDecoderSpec {
  type Json = _root_.io.circe.Json
  implicit val decoder = lspace.codec.circe.nativeDecoder

}
