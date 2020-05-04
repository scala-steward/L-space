package lspace.codec.circe

import io.circe.Json
import lspace.codec.json
import lspace.codec.json.geojson.{GeoJsonDecoder, GeoJsonEncoder}

class GeoJsonEncoderSpec extends json.geojson.EncoderSpec(lspace.codec.circe.nativeEncoder) {

  lazy val decoder        = lspace.codec.circe.nativeDecoder
  lazy val geojsondecoder = GeoJsonDecoder(decoder)
  lazy val geojsonencoder = GeoJsonEncoder(encoder)

  def parseRawToJson: String => Json = raw => io.circe.parser.parse(raw).toOption.get
}
