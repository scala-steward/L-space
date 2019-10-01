package lspace.codec.argonaut

import argonaut.Json
import lspace.codec.json
import lspace.codec.json.geojson.{GeoJsonDecoder, GeoJsonEncoder}

class GeoJsonEncoderSpec extends json.geojson.EncoderSpec(lspace.codec.argonaut.nativeEncoder) {

  lazy val decoder        = lspace.codec.argonaut.nativeDecoder
  lazy val geojsondecoder = GeoJsonDecoder(decoder)
  lazy val geojsonencoder = GeoJsonEncoder(encoder)

  def parseRawToJson: String => Json = raw => argonaut.Parse.parse(raw).toOption.get
}
