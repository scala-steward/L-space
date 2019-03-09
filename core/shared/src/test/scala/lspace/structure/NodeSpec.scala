package lspace.structure

import lspace.NS.types
import lspace.datatype.{DataType, VectorType}
import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, Matchers}
import lspace.util.SampleGraph

trait NodeSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll with GraphFixtures {
  import SampleGraph.ontologies._
  import SampleGraph.properties._

  def nodeTests(graph: Graph) = {
    "Nodes" can {
      "be queried by id" in {
        val node = graph.nodes.create(Property.ontology)
        node.addOut(Property.default.typed.iriUrlString, "abc")
        node.iri shouldBe "abc"
        node.out(Property.default.typed.iriUrlString).nonEmpty shouldBe true
        node.outE(Property.default.typed.iriUrlString).nonEmpty shouldBe true
        node.out(Property.default.typed.iriUrlString).head shouldBe "abc"
      }
      "be assigned an ontology" in {
        val node = graph.nodes.create()
        node.labels.size shouldBe 0
        node.addLabel(Ontology.ontology)
        node.labels.size shouldBe 1
      }
      "be assigned an unknown ontology" in {
        val node = graph.nodes.create()
        node.labels.size shouldBe 0
        node.addLabel(Ontology("veryunknownontology"))
        node.labels.size shouldBe 1
      }
      "be assigned an unknown ontology with unknown extended ontology" in {
        val node = graph.nodes.create()
        node.labels.size shouldBe 0
        lazy val veryunknownontology =
          Ontology("veryunknownontology")
        veryunknownontology.extendedClasses + veryunknownextendedontology
        lazy val veryunknownextendedontology =
          Ontology("veryunknownextendedontology")
        veryunknownextendedontology.extendedClasses + Ontology("veryveryunknownextendedontology")
        node.addLabel(veryunknownontology)
        veryunknownontology.extendedClasses().size shouldBe 1
        node.labels.size shouldBe 1
      }
      "be assigned two ontologies" in {
        val node = graph.nodes.create()
        node.labels.size shouldBe 0
        node.addLabel(Ontology.ontology)
        node.labels.size shouldBe 1
        node.addLabel(DataType.ontology)
        node.labels.size shouldBe 1
        node.labels.head shouldBe DataType.ontology
        node.labels.map(_.iri).contains(DataType.ontology.iri) shouldBe true
        node.addLabel(Ontology.ontology)
        node.labels.size shouldBe 1
        node.labels.head shouldBe DataType.ontology
        node.labels.map(_.iri).contains(DataType.ontology.iri) shouldBe true
      }
      "be assigned a relation" in {
        val node = graph.nodes.create()
        node.addOut("unknownkeyisnotaproblem", 123)
        node.out("unknownkeyisnotaproblem").size shouldBe 1
      }
      "be removed by node-method" in {
        val node = graph.nodes.create()
        node.addOut(Property.default.typed.iriUrlString, "123456")
        node.out(Property.default.`@id`).size shouldBe 1
        node.remove()
        node.out(Property.default.`@label`).size shouldBe 0
      }
//      "out ..." in {
//        //      graph.ontology.outE().nonEmpty shouldBe true
//        //      graph.ontology.inE().nonEmpty shouldBe true
//      }
    }
    "Nodes" should {
      "not contain an ontology if the ontology is already inherited from another ontology" in {
        val node = graph.nodes.create()
        node.addLabel(DataType.ontology)
        node.labels.size shouldBe 1
        node.labels.head shouldBe DataType.ontology
        val someDataTypeOntology =
          Ontology("schema.example.com/weirddata")
        someDataTypeOntology.extendedClasses + DataType.ontology
        someDataTypeOntology.extendedClasses().size shouldBe 1

//        node.addLabel(someDataTypeOntology)
//        val storedSomeDataTypeOntology = emptyGraph.ns.ontologies.store(someDataTypeOntology)
//        storedSomeDataTypeOntology.labels.size shouldBe 1
//        node.labels.size shouldBe 1
      }
    }
    "Properties" can {
      "only be single for cardinality single" ignore {
        val singleProperty = Property("singleproperty")
        singleProperty.range + DataType.default.`@string`

        val node = graph.nodes.create()
        node.addOut(singleProperty, "123456")
        node.out(singleProperty).size shouldBe 1
        node.out(singleProperty).head shouldBe "123456"
        node.addOut(singleProperty, "1234567")
        node.out(singleProperty).size shouldBe 1
        node.out(singleProperty).head shouldBe "1234567"
      }
      "have a collection as a value" in {
        val vectorProperty = Property("some.vector")
        vectorProperty.range + VectorType(List(DataType.default.`@int`))
        val intVector = vectorProperty + VectorType(List(DataType.default.`@int`))
        val node      = graph.nodes.create()
        node.addOut(intVector, Vector(1, 2, 3, 4))
        node.out("some.vector") shouldBe List(Vector(1, 2, 3, 4))
      }
      "be of type double" in {
        val number = Property("number")
        number.range + DataType.default.`@double`
        val numberDouble = number + DataType.default.`@double`
        val node         = graph.nodes.create()
        node.addOut(numberDouble, 0.0)
        node.out(numberDouble).head.getClass shouldBe 0.0.getClass
      }
    }
    "Properties" must {
      "maintain order" in {
        val node = graph.nodes.create()
        node.addOut("number", 1)
        node.addOut("number", 2)
        node.addOut("number", 3)
        node.addOut("number", 4)
        node.out("number") shouldBe List(1, 2, 3, 4)
        node.out() shouldBe List(1, 2, 3, 4)
      }
    }
  }

  def sampledNodeTests(sampledGraph: SampledGraph) = {
    val sampleGraph = sampledGraph.graph

    val Talca    = sampleGraph.nodes.hasIri(sampleGraph.iri + "/place/34567").headOption
    val Garrison = sampleGraph.nodes.hasIri(sampleGraph.iri + "/person/56789").headOption
    val Yoshio   = sampleGraph.nodes.hasIri(sampleGraph.iri + "/person/123").headOption
    val YoshioName =
      Yoshio
        .map(_.iri)
        .flatMap(iri => sampleGraph.edges().find(e => e.key == name.property && e.from.iri == iri).headOption)

    "a node" should {
      "support .out(key)" in {
        Garrison.map(_.out(name)) shouldBe Some(List("Garrison"))
      }

      "support .outE(key)" in {
        Yoshio.map(_.outE(name)) shouldBe YoshioName.map(List(_))
      }
      "support .outMap(key)" in {
        Yoshio.map(_.outMap(name)) shouldBe Some(Map(SampleGraph.properties.name.property -> List("Yoshio")))
      }
    }
    "a value" should {
      "support collection-types" in {
        Talca.map(_.out(name)) shouldBe Some(List(List("Talca", "Tal Ca")))
        Talca.map(_.out(name)) should not be Some(List(List("Talca", "Tal Cac")))
      }
    }
  }
}
