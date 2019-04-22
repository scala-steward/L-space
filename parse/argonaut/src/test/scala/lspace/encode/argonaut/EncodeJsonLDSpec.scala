package lspace.encode.argonaut

class EncodeJsonLDSpec
    extends lspace.encode.EncodeJsonLDSpec(lspace.codec.jsonld.Encoder.apply(lspace.codec.argonaut.nativeEncoder)) {}
