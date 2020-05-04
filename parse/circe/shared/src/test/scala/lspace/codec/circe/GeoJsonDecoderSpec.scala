package lspace.codec.circe

import lspace.codec.json
import lspace.codec.json.geojson.GeoJsonDecoder

class GeoJsonDecoderSpec extends json.geojson.DecoderSpec(lspace.codec.circe.nativeDecoder) {

  lazy val geojsondecoder = GeoJsonDecoder(decoder)
}
