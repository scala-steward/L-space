package lspace.codec.jsonld

import org.scalatest.{AsyncWordSpec, Matchers}

trait NativeTypeDecoderSpec extends AsyncWordSpec with Matchers {
  type Json

  import lspace._
  import Implicits.Scheduler.global
  def decoder: lspace.codec.NativeTypeDecoder.Aux[Json]

  "An decoder" should {
    "decode literals" in {
      (for {
        _ <- decoder.parse("""5""").map(decoder.jsonToInt).map(_ shouldBe Some(5))
        _ <- decoder.parse("""5""").map(decoder.jsonToString).map(_ shouldBe None)
        _ <- decoder.parse("""5.6""").map(decoder.jsonToDouble).map(_ shouldBe Some(5.6))
        _ <- decoder.parse("""5""").map(decoder.jsonToLong).map(_ shouldBe Some(5L))
        _ <- decoder.parse("""true""").map(decoder.jsonToBoolean).map(_ shouldBe Some(true))
        _ <- decoder.parse(""""5.5"""").map(decoder.jsonToString).map(_ shouldBe Some("5.5"))
        _ <- decoder.parse(""""abc"""").map(decoder.jsonToString).map(_ shouldBe Some("abc"))
      } yield succeed).runToFuture
    }
  }
}
