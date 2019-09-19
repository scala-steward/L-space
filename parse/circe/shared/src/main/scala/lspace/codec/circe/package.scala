package lspace.codec

package object circe {
  type JsonEncoder = circe.Encoder
  //  implicit lazy val JsonEncoder = circe.Encoder
  implicit val nativeEncoder: lspace.codec.circe.Encoder = circe.Encoder
  type JsonDecoder = circe.Decoder
  //  implicit lazy val JsonDecoder = circe.Decoder
  implicit val nativeDecoder: lspace.codec.circe.Decoder = circe.Decoder
}
