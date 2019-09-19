package lspace.codec.json

package object jsonld {
  type JsonLDDecoder[T] = jsonld.Decoder[T]
  lazy val JsonLDDecoder = jsonld.Decoder
  type JsonLDEncoder[T] = jsonld.Encoder[T]
  lazy val JsonLDEncoder = jsonld.Encoder
}
