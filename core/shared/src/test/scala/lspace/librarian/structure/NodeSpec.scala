package lspace.librarian.structure

import lspace.NS.types
import lspace.librarian.datatype.VectorType
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import lspace.librarian.process.traversal.P
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.util.SampleGraph

trait NodeSpec extends WordSpec with Matchers with BeforeAndAfterAll {
  def graph: Graph

  override def beforeAll = {
    SampleGraph.loadSocial(graph)
  }

  "Nodes" can {
    "be queried by id" in {
      val node = graph.createNode(Property.ontology)
      node.addOut(Property.default.typed.iriUrlString, "abc")
      node.iri shouldBe "abc"
      node.out(Property.default.typed.iriUrlString).nonEmpty shouldBe true
      node.outE(Property.default.typed.iriUrlString).nonEmpty shouldBe true
      node.out(Property.default.typed.iriUrlString).head shouldBe "abc"
    }
    "be assigned an ontology" in {
      val node = graph.createNode()
      node.labels.size shouldBe 0
      node.addLabel(Ontology.ontology)
      node.labels.size shouldBe 1
    }
    "be assigned an unknown ontology" in {
      val node = graph.createNode()
      node.labels.size shouldBe 0
      node.addLabel("veryunknownontology")
      node.labels.size shouldBe 1
    }
    "be assigned an unknown ontology with unknown extended ontology" in {
      val node = graph.createNode()
      node.labels.size shouldBe 0
      lazy val veryunknownontology =
        Ontology("veryunknownontology")(_extendedClasses = () => List(veryunknownextendedontology))
      lazy val veryunknownextendedontology = Ontology("veryunknownextendedontology")(_extendedClasses = () =>
        List(Ontology("veryveryunknownextendedontology")))
      node.addLabel(veryunknownontology)
      veryunknownontology.extendedClasses.size shouldBe 1
      node.labels.size shouldBe 1
    }
    "be assigned two ontologies" in {
      val node = graph.createNode()
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
      val node = graph.createNode()
      node.addOut("unknownkeyisnotaproblem", 123)
      node.out("unknownkeyisnotaproblem").size shouldBe 1
    }
    "be removed by traversal" ignore {
      val node = graph.createNode()
      node.addOut(Property.default.typed.iriUrlString, "12345")
      graph.g.N().has(Property.default.iri, P.eqv("12345")).toList.size shouldBe 1
      graph.g.N().hasIri(node.iri).toList.size shouldBe 1
      graph.g.N(node).toList.size shouldBe 1
      graph.g.N(node).drop().iterate()
      graph.g.N().hasIri("12345").toList.size shouldBe 0
    }
    "be removed by node-method" in {
      val node = graph.createNode()
      node.addOut(Property.default.typed.iriUrlString, "123456")
      graph.g.N().hasIri(node.iri).toList.size shouldBe 1
      node.remove()
      graph.g.N().hasIri("123456").toList.size shouldBe 0
    }
    "out ..." in {
      //      graph.ontology.outE().nonEmpty shouldBe true
      //      graph.ontology.inE().nonEmpty shouldBe true
    }
  }
  "Nodes" should {
    "not contain an ontology if the ontology is already inherited from another ontology" in {
      val node = graph.createNode()
      node.addLabel(DataType.ontology)
      node.labels.size shouldBe 1
      node.labels.head shouldBe DataType.ontology
      val someDataTypeOntology =
        Ontology("schema.example.com/weirddata")(_extendedClasses = () => List(DataType.ontology))
      someDataTypeOntology.extendedClasses.size shouldBe 1

      node.addLabel(someDataTypeOntology)
      val storedSomeDataTypeOntology = graph.ns.storeOntology(someDataTypeOntology)
      storedSomeDataTypeOntology.labels.size shouldBe 1
      node.labels.size shouldBe 1
    }
  }
  "Properties" can {
    "only be single for cardinality single" ignore {
      val singleProperty = Property("singleproperty")(_range = () => List(DataType.default.textType))

      val node = graph.createNode()
      node.addOut(singleProperty, "123456")
      node.out(singleProperty).size shouldBe 1
      node.out(singleProperty).head shouldBe "123456"
      node.addOut(singleProperty, "1234567")
      node.out(singleProperty).size shouldBe 1
      node.out(singleProperty).head shouldBe "1234567"
    }
    "be many and contain duplicates for cardinality list" ignore {
      val listProperty =
        Property("listproperty")(_range = () => List(DataType.default.textType), containers = List(types.list))

      val node = graph.createNode()
      node.addOut(listProperty, "123456")
      node.out(listProperty).size shouldBe 1
      node.out(listProperty).head shouldBe "123456"
      node.addOut(listProperty, "1234567")
      node.out(listProperty).size shouldBe 2
      node.out(listProperty) should contain("1234567")
      node.addOut(listProperty, "1234567")
      node.out(listProperty).size shouldBe 3
      node.out(listProperty) should contain("1234567")
    }
    "only be unique and contain no duplicates for cardinality set" ignore {
      val listProperty =
        Property("setproperty")(_range = () => List(DataType.default.textType), containers = List(types.set))

      val node = graph.createNode()
      node.addOut(listProperty, "123456")
      node.out(listProperty).size shouldBe 1
      node.out(listProperty).head shouldBe "123456"
      node.addOut(listProperty, "1234567")
      node.out(listProperty).size shouldBe 2
      node.out(listProperty) should contain("1234567")
      node.addOut(listProperty, "1234567")
      node.out(listProperty).size shouldBe 2
      node.out(listProperty) should contain("1234567")
    }
    "have a collection as a value" in {
      val vectorProperty = Property("some.vector")(_range = () => List(VectorType(List(DataType.default.intType))))
      val intVector      = vectorProperty + DataType.default.intType
      val node           = graph.createNode()
      node.addOut(vectorProperty, Vector(1, 2, 3, 4))
      node.out("some.vector") shouldBe List(Vector(1, 2, 3, 4))
    }
    "be of type double" in {
      val number       = Property("number")(_range = () => List(DataType.default.doubleType), containers = List(types.set))
      val numberDouble = number + DataType.default.doubleType
      val node         = graph.createNode()
      node.addOut(numberDouble, 0.0)
      node.out(numberDouble).head.getClass shouldBe 0.0.getClass
    }
  }
}
