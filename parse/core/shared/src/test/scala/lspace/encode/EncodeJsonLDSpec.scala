package lspace.encode

import lspace.Property
import lspace.codec.json.jsonld.JsonLDEncoder
import lspace.codec.{ActiveContext, ActiveProperty, NamedActiveContext}
import monix.eval.Coeval
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ListMap

abstract class EncodeJsonLDSpec[Json](encoder: JsonLDEncoder[Json]) extends AnyWordSpec with Matchers {

  implicit val enc = encoder
  import encoder._
  import EncodeJsonLD._

  "EncodeJsonLD" should {
    "encode an empty ActiveContext" in {
      val defaultContext = ActiveContext()
      implicitly[EncodeJsonLD[ActiveContext, Coeval]]
        .encode(ActiveContext())(defaultContext)
        .value() shouldBe """{"@context":{}}"""
    }
    "encode an ActiveContext with remote context" in {
      val defaultContext =
        ActiveContext(remotes = List(NamedActiveContext("https://remote.example.org", ActiveContext())))

      implicitly[EncodeJsonLD[ActiveContext, Coeval]]
        .encode(ActiveContext())(defaultContext)
        .value() shouldBe """{"@context":"https://remote.example.org"}"""
    }
    "encode an ActiveContext with remote context and a local context" in {
      val defaultContext = ActiveContext(`@prefix` = ListMap("name" -> "https://example.com/name"),
                                         remotes =
                                           List(NamedActiveContext("https://remote.example.org", ActiveContext())))

      implicitly[EncodeJsonLD[ActiveContext, Coeval]]
        .encode(ActiveContext())(defaultContext)
        .value() shouldBe """{"@context":["https://remote.example.org",{"name":"https://example.com/name"}]}"""
    }
  }
}
