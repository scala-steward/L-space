package lspace.codec.argonaut

import lspace.codec.json.jsonld
import lspace.structure.Graph

class JsonLDDecoderSpec extends jsonld.JsonLDDecoderSpec(jsonld.Decoder(Graph("DecoderSpec"))) {
  val x = lspace.parse.test.g //work-around to get AbortController loaded in NodeJS, this is only for test-scope, not packaging

}
