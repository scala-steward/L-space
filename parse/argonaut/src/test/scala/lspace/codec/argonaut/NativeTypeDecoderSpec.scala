package lspace.codec.argonaut

class NativeTypeDecoderSpec extends lspace.codec.jsonld.NativeTypeDecoderSpec {
  type Json = _root_.argonaut.Json
  implicit val decoder = lspace.codec.argonaut.nativeDecoder

}
