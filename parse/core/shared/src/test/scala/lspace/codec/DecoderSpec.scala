package lspace.codec

import org.scalatest.{AsyncWordSpec, Matchers}

trait DecoderSpec[Json] extends AsyncWordSpec with Matchers {
  def decoder: lspace.codec.Decoder
}
