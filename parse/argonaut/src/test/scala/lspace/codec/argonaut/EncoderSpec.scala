package lspace.codec.argonaut

import argonaut.Json

class EncoderSpec extends lspace.codec.EncoderSpec[Json] {
  implicit val encoder = Encoder
}
