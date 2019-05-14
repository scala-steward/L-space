package lspace.structure

import lspace.librarian.traversal.step.V
import monix.eval.Task
import org.scalatest.{AsyncWordSpec, Matchers}

class OntologySpec extends AsyncWordSpec with Matchers {
  import lspace.Implicits.Scheduler.global

  "Ontologies" can {
    "be compared by iri" in {
      new Ontology("abc") shouldBe new Ontology("abc")
      new Ontology("abc") should not be new Ontology("abcd")

      val ontologyABC = new Ontology("abc")
      List(ontologyABC, ontologyABC, ontologyABC).toSet.size shouldBe 1

      val ontologyABCD = new Ontology("abcd")
      List(ontologyABC, ontologyABC, ontologyABCD).toSet.size shouldBe 2
    }
  }

  "An ontology" can {
    "extend some other ontology" in {
      val ontology = V.ontology
      ontology.extendedClasses().size shouldBe 1
    }
  }
  "An ontology.properties" should {
    ".+ thread-safe" in {
      val p = new Ontology("a")
      (for {
        _ <- Task.gatherUnordered {
          (1 to 1000).map(i => new Property(s"a$i")).map(p.properties.+(_)).map(Task.now)
        }
      } yield p.properties().size shouldBe 1000).runToFuture
    }
    ".++ thread-safe" in {
      val p = new Property("a")
      (for {
        _ <- Task.gatherUnordered {
          (1 to 1000).map(i => new Property(s"a$i")).grouped(100).map(p.properties.++(_)).map(Task.now)
        }
      } yield p.properties().size shouldBe 1000).runToFuture
    }
  }
}
