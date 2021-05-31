package lspace.structure

import lspace._
import lspace.librarian.traversal.step.V
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class OntologySpec extends AsyncWordSpec with Matchers {
  override def executionContext = lspace.Implicits.Scheduler.global

  "Ontologies".can {
    "be compared by iri" in {
      new Ontology("abc") shouldBe new Ontology("abc")
      new Ontology("abc") should not be new Ontology("abcd")

      val ontologyABC = new Ontology("abc")
      List(ontologyABC, ontologyABC, ontologyABC).toSet.size shouldBe 1

      val ontologyABCD = new Ontology("abcd")
      List(ontologyABC, ontologyABC, ontologyABCD).toSet.size shouldBe 2
    }
  }

  "An ontology".can {
    "extend some other ontology" in {
      val ontology = V.ontology
      ontology.extendedClasses().size shouldBe 1
    }
    "extends can be circular" in {
      val a: Ontology = "a"
      val b: Ontology = "b"
      val c: Ontology = "c"
      val d: Ontology = "d"
      a.`@extends`(b) shouldBe false
      a.extendedClasses.+(b)
      a.`@extends`(b) shouldBe true
      b.`@extends`(c) shouldBe false
      b.extendedClasses.+(c)
      b.`@extends`(c) shouldBe true
      a.`@extends`(c) shouldBe true
      a.extendedClasses.+(c)
      a.`@extends`(c) shouldBe true
      b.`@extends`(b) shouldBe false
      b.extendedClasses.+(b)
      b.`@extends`(b) shouldBe true
      a.`@extends`(a) shouldBe false
      c.`@extends`(a) shouldBe false
      c.extendedClasses.+(a)
      c.`@extends`(a) shouldBe true
      a.`@extends`(a) shouldBe true
      a.`@extends`(d) shouldBe false
      b.`@extends`(d) shouldBe false
      a.extendedClasses.+(d)
      a.`@extends`(d) shouldBe true
      b.`@extends`(d) shouldBe true
    }
  }
//  "An ontology.properties" should {
//    ".+ thread-safe" in {
//      val p = new Ontology("a")
//      (for {
//        _ <- Task.parSequenceUnordered {
//          (1 to 1000).map(i => new Property(s"a$i")).map(p.properties.+(_)).map(Task.now)
//        }
//      } yield p.properties().size shouldBe 1000).runToFuture
//    }
//    ".++ thread-safe" in {
//      val p = new Property("a")
//      (for {
//        _ <- Task.parSequenceUnordered {
//          (1 to 1000).map(i => new Property(s"a$i")).grouped(100).map(p.properties.++(_)).map(Task.now).toIterable
//        }
//      } yield p.properties().size shouldBe 1000).runToFuture
//    }
//  }
}
