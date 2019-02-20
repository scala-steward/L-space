package lspace.structure

import org.scalatest.{Matchers, WordSpec}

class PropertySpec extends WordSpec with Matchers {

  "Properties" can {
    "be compared by iri" in {
      Property("abc") shouldBe Property("abc")
      Property("abc") should not be Property("abcd")

      val propertyABC = Property("abc")
      List(propertyABC, propertyABC, propertyABC).toSet.size shouldBe 1

      val propertyABCD = Property("abcd")
      List(propertyABC, propertyABC, propertyABCD).toSet.size shouldBe 2
    }
  }
}
