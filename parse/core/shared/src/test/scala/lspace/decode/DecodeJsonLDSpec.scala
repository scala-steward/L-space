package lspace.decode

import lspace.codec.json.jsonld.JsonLDDecoder
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

abstract class DecodeJsonLDSpec[Json](decoder: JsonLDDecoder[Json]) extends AsyncWordSpec with Matchers {

  implicit val dec = decoder

}
