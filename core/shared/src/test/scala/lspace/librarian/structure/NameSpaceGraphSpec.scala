package lspace.librarian.structure

import lspace.NS.types
import lspace.librarian.datatype.DataType
import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, Matchers}

trait NameSpaceGraphSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {
  implicit val ec = monix.execution.Scheduler.global

  def nameSpaceGraphTests(graph: Graph) =
    "a namespace graph" must {
      "have an ontologies API" which {}
      "have an properties API" which {}
      "have a datatypes API" which {}
      "have a classtypes API" which {}

      "be provided with default ontologies, properties and datatypes" in {
        graph.ns.ontologies.cached(types.`@class`).isDefined shouldBe true
        graph.ns.ontologies.cached(types.`@property`).isDefined shouldBe true
        graph.ns.ontologies.cached(types.`@datatype`).isDefined shouldBe true

        graph.ns.properties.cached(types.`@id`).isDefined shouldBe true
        graph.ns.properties.cached(types.`@label`).isDefined shouldBe true

        graph.ns.datatypes.cached(types.`@string`).isDefined shouldBe true
        graph.ns.datatypes.cached(types.xsdDateTime).isDefined shouldBe true

        graph.ns.classtypes.cached(types.xsdDateTimeStamp).isDefined shouldBe true
        graph.ns.classtypes.cached(types.`@comment`).isDefined shouldBe true
        graph.ns.classtypes.cached(types.`@property`).isDefined shouldBe true
      }
      "store and retrieve an ontology" in {
        val unknownOntology = Ontology("unknownOntology", extendedClasses = List(DataType.ontology))
        graph.ns.ontologies.cached(unknownOntology.iri).isEmpty shouldBe true

        graph.ns.ontologies
          .store(unknownOntology)
          .map { node =>
            node.out(Property.default.`@extends`).size shouldBe 1
            node.labels.size shouldBe 1
            node.iri shouldBe unknownOntology.iri

            graph.ns.ontologies.cached(unknownOntology.iri).isDefined shouldBe true
          }
          .runToFuture
      }
    }
}
