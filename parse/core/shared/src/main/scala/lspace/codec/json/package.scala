package lspace.codec

package object json {
  type JsonDecoder[T] = lspace.codec.json.Decoder[T]
  lazy val JsonDecoder = lspace.codec.json.Decoder
  type JsonEncoder[T] = lspace.codec.json.Encoder[T]
  lazy val JsonEncoder = lspace.codec.json.Encoder
}
