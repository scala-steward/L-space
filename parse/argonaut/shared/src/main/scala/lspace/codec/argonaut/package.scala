package lspace.codec

package object argonaut {
  type JsonEncoder = argonaut.Encoder
//  implicit lazy val JsonEncoder = argonaut.Encoder
  implicit val nativeEncoder: lspace.codec.argonaut.Encoder = argonaut.Encoder
  type JsonDecoder = argonaut.Decoder
//  implicit lazy val JsonDecoder = argonaut.Decoder
  implicit val nativeDecoder: lspace.codec.argonaut.Decoder = argonaut.Decoder
}
