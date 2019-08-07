package lspace.structure

import lspace._
import lspace.datatype.{DataType, VectorType}
import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, Matchers}
import lspace.util.SampleGraph
import monix.eval.Task
import monix.execution.Scheduler

trait NodeSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll with GraphFixtures {
  import SampleGraph.ontologies._
  import SampleGraph.properties._
  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

  def nodeTests(graph: Graph) = {
    "Nodes" can {
      "be queried by id" in {
        (for {
          node <- graph.nodes.create(Property.ontology)
          _    <- node.addOut(Property.default.typed.iriUrlString, "abc")
        } yield {
          node.iri shouldBe "abc"
          node.out(Property.default.typed.iriUrlString).nonEmpty shouldBe true
          node.outE(Property.default.typed.iriUrlString).nonEmpty shouldBe true
          node.out(Property.default.typed.iriUrlString).head shouldBe "abc"
        }).runToFuture
      }
      "be assigned an ontology" in {
        (for {
          node <- graph.nodes.create()
          _ = node.labels.size shouldBe 0
          _ <- node.addLabel(Ontology.ontology)
          _ = node.labels.size shouldBe 1
        } yield succeed).runToFuture
      }
      "be assigned an unknown ontology" in {
        (for {
          node <- graph.nodes.create()
          _ = node.labels.size shouldBe 0
          _ <- node.addLabel(Ontology("veryunknownontology"))
          _ = node.labels.size shouldBe 1
        } yield succeed).runToFuture
      }
      "be assigned an unknown ontology with unknown extended ontology" in {
        lazy val veryunknownontology =
          Ontology("veryunknownontology")
        veryunknownontology.extendedClasses + veryunknownextendedontology
        lazy val veryunknownextendedontology =
          Ontology("veryunknownextendedontology")
        veryunknownextendedontology.extendedClasses + Ontology("veryveryunknownextendedontology")
        veryunknownontology.extendedClasses().size shouldBe 1
        (for {
          node <- graph.nodes.create()
          _ = node.labels.size shouldBe 0
          _ <- node.addLabel(veryunknownontology)
          _ = node.labels.size shouldBe 1
        } yield succeed).runToFuture
      }
      "be assigned two ontologies" in {
        (for {
          node <- graph.nodes.create()
          _ = node.labels.size shouldBe 0
          _ <- node.addLabel(Ontology.ontology)
          _ = node.labels.size shouldBe 1
          _ <- node.addLabel(DataType.ontology)
          _ = node.labels.size shouldBe 1
          _ = node.labels.head shouldBe DataType.ontology
          _ = node.labels.map(_.iri).contains(DataType.ontology.iri) shouldBe true
          _ <- node.addLabel(Ontology.ontology)
          _ = node.labels.size shouldBe 1
          _ = node.labels.head shouldBe DataType.ontology
          _ = node.labels.map(_.iri).contains(DataType.ontology.iri) shouldBe true
        } yield succeed).runToFuture
      }
      "be assigned a relation" in {
        (for {
          node <- graph.nodes.create()
          _    <- node.addOut("unknownkeyisnotaproblem", 123)
        } yield {
          node.out("unknownkeyisnotaproblem").size shouldBe 1
        }).runToFuture
      }
      "be removed by node-method" in {
        (for {
          node <- graph.nodes.create()
          _    <- node.addOut(Property.default.typed.iriUrlString, "123456")
          _ = node.out(Property.default.`@id`).size shouldBe 1
          _ <- node.remove()
          _ = node.out(Property.default.`@label`).size shouldBe 0
        } yield succeed).runToFuture
      }
//      "out ..." in {
//        //      graph.ontology.outE().nonEmpty shouldBe true
//        //      graph.ontology.inE().nonEmpty shouldBe true
//      }
    }
    "Nodes" should {
      "not contain an ontology if the ontology is already inherited from another ontology" in {
        (for {
          node <- graph.nodes.create()
          _    <- node.addLabel(DataType.ontology)
        } yield {
          node.labels.size shouldBe 1
          node.labels.head shouldBe DataType.ontology
          val someDataTypeOntology =
            Ontology("schema.example.com/weirddata")
          someDataTypeOntology.extendedClasses + DataType.ontology
          someDataTypeOntology.extendedClasses().size shouldBe 1
        }).runToFuture

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

        (for {
          node <- graph.nodes.create()
          _    <- node.addOut(singleProperty, "123456")
          a = {
            node.out(singleProperty).size shouldBe 1
            node.out(singleProperty).head shouldBe "123456"
          }
          _ <- node.addOut(singleProperty, "1234567")
        } yield {
          node.out(singleProperty).size shouldBe 1
          node.out(singleProperty).head shouldBe "1234567"
        }).runToFuture
      }
      "have a collection as a value" in {
        val vectorProperty = Property("some.vector")
//        vectorProperty as VectorType(DataType.default.`@int`)
        val intVector = vectorProperty as VectorType(DataType.default.`@int`)
        (for {
          node <- graph.nodes.create()
          _    <- node.addOut(intVector, Vector(1, 2, 3, 4))
        } yield {
          node.out("some.vector") shouldBe List(Vector(1, 2, 3, 4))
        }).runToFuture
      }
      "be of type double" in {
        val number = Property("number")
        number.range + DataType.default.`@double`
        val numberDouble = number as DataType.default.`@double`
        (for {
          node <- graph.nodes.create()
          _    <- node.addOut(numberDouble, 0.0)
        } yield {
          node.out(numberDouble).head.getClass shouldBe 0.0.getClass
        }).runToFuture
      }
    }
    "Properties" must {
      "maintain order" in {
        (for {
          node <- graph.nodes.create()
          _    <- node.addOut("number", 1)
          _    <- node.addOut("number", 2)
          _    <- node.addOut("number", 3)
          _    <- node.addOut("number", 4)
        } yield {
          node.out("number").toSet shouldBe Set(1, 2, 3, 4)
          node.out().toSet shouldBe Set(1, 2, 3, 4)
        }).runToFuture
      }
    }
  }

  def sampledNodeTests(sampledGraph: SampledGraph) = {
    val sampleGraph = sampledGraph.graph

    lazy val Talca    = sampleGraph.nodes.hasIri(sampleGraph.iri + "/place/34567").headL.memoizeOnSuccess
    lazy val Garrison = sampleGraph.nodes.hasIri(sampleGraph.iri + "/person/56789").headL.memoizeOnSuccess
    lazy val Yoshio   = sampleGraph.nodes.hasIri(sampleGraph.iri + "/person/123").headL.memoizeOnSuccess
    lazy val YoshioName = for {
      y <- Yoshio
      e <- sampleGraph.edges().find(e => e.key == name.property && e.from.iri == y.iri).headL
    } yield e

    "a node" should {
      "support .out(key)" in {
        Garrison.map(_.out(name) shouldBe List("Garrison")).runToFuture
      }

      "support .outE(key)" in {
        Task.parMap2(Yoshio.map(_.outE(name)), YoshioName.map(List(_))) { case (a, b) => a shouldBe b }.runToFuture
      }
      "support .outMap(key)" in {
        Yoshio
          .map(_.outMap(name) shouldBe Map(SampleGraph.properties.name.property -> List("Yoshio")))
          .runToFuture
      }
    }
    "a value" should {
      "support collection-types" in {
        Talca.map(_.out(name) shouldBe List(List("Talca", "Tal Ca"))).runToFuture
        Talca.map(_.out(name) should not be List(List("Talca", "Tal Cac"))).runToFuture
      }
    }
  }
}
