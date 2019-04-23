package lspace.encode

import lspace.Property
import lspace.codec.{ActiveContext, ActiveProperty}
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
      implicitly[EncodeJsonLD[ActiveContext]].encode(ActiveContext())(defaultContext) shouldBe """"@context": {}"""
    }
  }
}
