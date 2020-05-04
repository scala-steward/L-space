package lspace.librarian.logic.predicate

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.List

class PSpec extends AnyWordSpec with Matchers {

  "A P".can {
    "consist of multiple steps" in {
      (P.eqv("a") || P.eqv(1)) shouldBe Or(List(P.eqv("a"), P.eqv(1)))
      (P.eqv("a") && P.eqv(1)) shouldBe And(List(P.eqv("a"), P.eqv(1)))
    }
    "can hold nested structures (e.g. Contains(Eqv(1.0)) asserts true a collection like List(1) or List(1.0) or List(2, 1)" in {
      P.contains(P.eqv(1.0))
    }
  }
}
