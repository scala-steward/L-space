package lspace.codec.json

package object geojson {
  type GeoJsonDecoder[T] = geojson.Decoder[T]
  lazy val GeoJsonDecoder = geojson.Decoder
}
