package lspace.codec.argonaut

import lspace.structure.Graph

class DecoderSpec extends lspace.codec.jsonld.DecoderSpec(lspace.codec.jsonld.Decoder(Graph("DecoderSpec"))) {
  val decoder: lspace.codec.jsonld.Decoder = lspace.codec.jsonld.Decoder(graph)

}
