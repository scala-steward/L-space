package lspace.structure

import lspace.datatype.DataType
import lspace.Label.D._
import org.scalatest.{AsyncWordSpec, Matchers}

class DataTypeSpec extends AsyncWordSpec with Matchers {

  "DataType" should {
    "dt" in {
      DataType.datatypes.get("http://schema.org/Text") shouldBe Some(`@string`)
    }
  }
}
