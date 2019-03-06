package lspace.codec.argonaut

import lspace.structure.Graph

class DecoderSpec extends lspace.codec.DecoderSpec {
  val graph: Graph                  = Graph("DecoderSpec")
  val decoder: lspace.codec.Decoder = lspace.codec.Decoder(graph)

}
