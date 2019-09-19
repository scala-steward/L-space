package lspace.encode.argonaut

class EncodeJsonLDSpec
    extends lspace.encode.EncodeJsonLDSpec(lspace.codec.json.jsonld.Encoder.apply(lspace.codec.argonaut.nativeEncoder)) {
  val x = lspace.parse.test.g //work-around to get AbortController loaded in NodeJS, this is only for test-scope, not packaging

}
