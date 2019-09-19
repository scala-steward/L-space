package lspace.codec.argonaut

import lspace.codec.json

class JsonDecoderSpec extends json.JsonDecoderSpec {
  type Json = _root_.argonaut.Json
  implicit val decoder = lspace.codec.argonaut.nativeDecoder

}
