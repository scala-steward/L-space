package lspace

import lspace.codec.json.jsonld.JsonLDDecoder
import org.scalatest.{AsyncWordSpec, Matchers}

abstract class DecodeJsonLDSpec[Json](decoder: JsonLDDecoder[Json]) extends AsyncWordSpec with Matchers {

  implicit val dec = decoder
  import decoder._
  import lspace.decode.DecodeJsonLD._

}
