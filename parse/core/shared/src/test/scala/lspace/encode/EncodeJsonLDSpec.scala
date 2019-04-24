package lspace.encode

import lspace.Property
import lspace.codec.{ActiveContext, ActiveProperty, NamedActiveContext}
import lspace.codec.jsonld.Encoder
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.ListMap

abstract class EncodeJsonLDSpec(encoder: Encoder) extends WordSpec with Matchers {

  implicit val enc = encoder
  import encoder._
  import EncodeJsonLD._

  "EncodeJsonLD" should {
    "encode an empty ActiveContext" in {
      val defaultContext = ActiveContext()
      implicitly[EncodeJsonLD[ActiveContext]].encode(ActiveContext())(defaultContext) shouldBe """{"@context":{}}"""
    }
    "encode an ActiveContext with remote context" in {
      val defaultContext =
        ActiveContext(remotes = List(NamedActiveContext("https://remote.example.org", ActiveContext())))

      implicitly[EncodeJsonLD[ActiveContext]]
        .encode(ActiveContext())(defaultContext) shouldBe """{"@context":"https://remote.example.org"}"""
    }
    "encode an ActiveContext with remote context and a local context" in {
      val defaultContext = ActiveContext(`@prefix` = ListMap("name" -> "https://example.com/name"),
                                         remotes =
                                           List(NamedActiveContext("https://remote.example.org", ActiveContext())))

      implicitly[EncodeJsonLD[ActiveContext]]
        .encode(ActiveContext())(defaultContext) shouldBe """{"@context":["https://remote.example.org",{"name":"https://example.com/name"}]}"""
    }
  }
}
