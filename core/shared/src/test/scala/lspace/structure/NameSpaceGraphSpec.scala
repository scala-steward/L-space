package lspace.structure

import lspace.NS.types
import lspace.datatype.DataType
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
        graph.ns.datatypes.cached(types.xsdDateTimeStamp).isDefined shouldBe true

        graph.ns.classtypes.cached(types.xsdDateTimeStamp).isDefined shouldBe true
        graph.ns.classtypes.cached(types.`@comment`).isDefined shouldBe true
        graph.ns.classtypes.cached(types.`@property`).isDefined shouldBe true
      }
      "store and retrieve an ontology" in {
        val unknownOntology = Ontology("unknownOntology")
        unknownOntology.extendedClasses + DataType.ontology
        graph.ns.ontologies.cached(unknownOntology.iri).isDefined shouldBe true

        graph.ns.ontologies
          .store(unknownOntology)
          .map { node =>
            node.out(Property.default.`@extends`).size shouldBe 1
            node.labels.size shouldBe 1
            node.iri shouldBe unknownOntology.iri

            graph.ns.ontologies.all.contains(unknownOntology) shouldBe true
            graph.ns.nodes.hasIri(unknownOntology.iri).contains(node) shouldBe true
          }
          .runToFuture
      }
      "store and retrieve a property" in {
        val unknownProperty = Property("new_property")
        graph.ns.properties
          .store(unknownProperty)
          .map { node =>
            node.labels.size shouldBe 1
            node.iri shouldBe unknownProperty.iri

            graph.ns.properties.cached(unknownProperty.iri).isDefined shouldBe true
            graph.ns.properties.all.contains(unknownProperty) shouldBe true
            graph.ns.nodes.hasIri(unknownProperty.iri).contains(node) shouldBe true
          }
          .runToFuture
      }
    }
}
