package lspace.codec.circe

import lspace.codec.json.jsonld

class JsonLDEncoderSpec extends jsonld.JsonLDEncoderSpec(jsonld.Encoder.apply) {}
