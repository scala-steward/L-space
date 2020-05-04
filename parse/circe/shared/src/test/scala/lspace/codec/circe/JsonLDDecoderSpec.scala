package lspace.codec.circe

import lspace.codec.json.jsonld
import lspace.structure.Graph

class JsonLDDecoderSpec extends jsonld.JsonLDDecoderSpec(jsonld.Decoder(Graph("DecoderSpec"))) {

  try {
    lspace.parse.test
      .init() //work-around to get AbortController loaded in NodeJS, this is only for test-scope, not packaging
  } catch {
    case e: Throwable => e.printStackTrace()
  }
}