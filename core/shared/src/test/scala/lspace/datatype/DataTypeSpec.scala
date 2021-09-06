package lspace.datatype

import lspace.Label.D._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DataTypeSpec extends AnyWordSpec with Matchers {

  "merging datatypes" should {
    "result in Any for @tuple(@int, @double) + @tuple(@int, @double, @long)" in {
      (`@tuple`(`@int`, `@double`) + `@tuple`(`@int`, `@double`, `@long`)) shouldBe `@tuple`()
    }
    "result in @number for @int + @double" in {
      `@int` + `@double` shouldBe `@number`
    }

  }
  "retrieving datatypes" should {
    "dt" in {
      DataType.datatypes.get("https://schema.org/Text") shouldBe Some(`@string`)
    }
  }
}
