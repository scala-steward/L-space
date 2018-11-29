package lspace.librarian.structure

import lspace.librarian.process.traversal.step.V
import org.scalatest.{Matchers, WordSpec}

class OntologySpec extends WordSpec with Matchers {

  "Ontologies" can {
    "be compared by iri" in {
      Ontology("abc") shouldBe Ontology("abc")
      Ontology("abc") should not be Ontology("abcd")

      val ontologyABC = Ontology("abc")
      List(ontologyABC, ontologyABC, ontologyABC).toSet.size shouldBe 1

      val ontologyABCD = Ontology("abcd")
      List(ontologyABC, ontologyABC, ontologyABCD).toSet.size shouldBe 2
    }
  }

  "An ontology" can {
    "extend some other ontology" in {
      val ontology = V.ontology
      ontology.extendedClasses.size shouldBe 1
    }
  }
}
