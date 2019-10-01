package lspace.codec.argonaut

import lspace.codec.json
import lspace.codec.json.geojson.GeoJsonDecoder

class GeoJsonDecoderSpec extends json.geojson.DecoderSpec(lspace.codec.argonaut.nativeDecoder) {

  lazy val geojsondecoder = GeoJsonDecoder(decoder)
}
