package lspace.librarian.process.traversal

import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure.Ontology
import org.scalatest.{Matchers, WordSpec}

class PSpec extends WordSpec with Matchers {
  MemGraphDefault.iri

  "A P" can {
    "consist of multiple steps" in {
      P.eqv(1.0).assert(1) shouldBe true
      P.eqv(1.0).assert(2) shouldBe false
      P.gt("aa").assert(2) shouldBe false
      P.gt("aa").assert("aa") shouldBe false
    }
    "is of a uniform type" in {
      P.eqv(Ontology.ontology.iri).assert("a") shouldBe false
      P.eqv(Ontology.ontology.iri).assert(Ontology.ontology.iri) shouldBe true
      P.between(1.0, 4l).assert(2) shouldBe true
      P.between(1.0, 4l).assert(5) shouldBe false
      P.within(1.0, "a").assert("b") shouldBe false
      P.within(1.0, "a").assert("a") shouldBe true
      P.within(1.0, "a").assert(1) shouldBe true
      P.within(1.0, "a").assert(1.0) shouldBe true
      //      P.eq(1.0).neq("a").steps.size shouldBe 2 does not and should never compile
    }
  }
  //  "P's" can {
  //    "be concatenated" in {
  //      (P.eq(1.0) ++ P.neq(1.1)).steps.size shouldBe 2
  //      (P.eq(1.0) ++ P.neq(1.1) ++ P.neq(1.2)).steps.size shouldBe 3
  //    }
  //  }
}
