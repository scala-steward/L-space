package lspace

import lspace.codec.jsonld.Decoder
import org.scalatest.{AsyncWordSpec, Matchers}

abstract class DecodeJsonLDSpec(decoder: Decoder) extends AsyncWordSpec with Matchers {

  implicit val dec = decoder
  import decoder._
  import lspace.decode.DecodeJsonLD._

}
