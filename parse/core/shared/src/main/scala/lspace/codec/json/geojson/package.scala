package lspace.codec.json

package object geojson {
  type GeoJsonDecoder[T] = geojson.Decoder[T]
  lazy val GeoJsonDecoder = geojson.Decoder
  type GeoJsonEncoder[T] = geojson.Encoder[T]
  lazy val GeoJsonEncoder = geojson.Encoder
}
