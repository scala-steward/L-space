package lspace.types.vector

import org.scalatest.{Matchers, WordSpec}

class VectorSpec extends WordSpec with Matchers {

  "A BBox(5,5,10,10)" should {
    "be within BBox(4,4,11,11)" in {
      BBox(5, 5, 10, 10) within BBox(4, 4, 11, 11) shouldBe true
    }
    "not be within BBox(6, 6, 11, 11) or BBox(6, 6, 9, 9)" in {
      BBox(5, 5, 10, 10) within BBox(6, 6, 11, 11) shouldBe false
      BBox(5, 5, 10, 10) within BBox(6, 6, 9, 9) shouldBe false
    }
    "contain BBox(6,6,9,9)" in {
      BBox(5, 5, 10, 10) contains BBox(6, 6, 9, 9) shouldBe true
    }
    "not contain BBox(4,4,11,11) or BBox(4,4,9,9)" in {
      BBox(5, 5, 10, 10) contains BBox(4, 4, 11, 11) shouldBe false
      BBox(5, 5, 10, 10) contains BBox(4, 4, 9, 9) shouldBe false
    }
  }

  "A Point(4,4)" should {
    "be within Polygon((2,2),(2,6),(6,6),(6,2))" in {
      Point(4, 4) within Polygon((2, 2), (2, 6), (6, 6), (6, 2)) shouldBe true
    }

    "not be within Polygon((5,5),(5,6),(6,6),(6,5))" in {
      Point(4, 4) within Polygon((5, 5), (5, 6), (6, 6), (6, 5)) shouldBe false
    }
  }

  "A Polygon((2,2),(2,6),(6,6),(6,2))" should {
    "contain Point(4,4)" in {
      Polygon((2, 2), (2, 6), (6, 6), (6, 2)) contains Point(4, 4) shouldBe true
    }
    "not contain Point(1,2)" in {
      Polygon((2, 2), (2, 6), (6, 6), (6, 2)) contains Point(1, 2) shouldBe false
    }
  }
}
