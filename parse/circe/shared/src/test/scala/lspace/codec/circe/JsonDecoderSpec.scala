package lspace.codec.circe

import lspace.codec.json

class JsonDecoderSpec extends json.JsonDecoderSpec {
//  val x = lspace.parse.test.g //work-around to get AbortController loaded in NodeJS, this is only for test-scope, not packaging
//  lspace.parse.test

  type Json = _root_.io.circe.Json
  implicit val decoder = lspace.codec.circe.nativeDecoder

}
