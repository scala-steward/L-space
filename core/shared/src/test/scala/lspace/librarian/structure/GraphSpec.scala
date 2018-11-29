package lspace.librarian.structure

import java.time.Instant

import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.step.V
import lspace.NS.types
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.util.SampleGraph
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

trait GraphSpec extends WordSpec with Matchers with BeforeAndAfterAll {
  def graph: Graph
  def sampleGraph: Graph
  def createGraph(iri: String): Graph
  def createSampleGraph(iri: String): Graph = {
    val graph = createGraph(iri)
    SampleGraph.loadSocial(graph)
    graph
  }

  override def beforeAll = SampleGraph.loadSocial(sampleGraph)

  "idProvider" should {
    "provide an unused id on .next" in {
      val nextId = sampleGraph.idProvider.next
      sampleGraph.resources.hasId(nextId - 5).orElse(sampleGraph.ns.resources.hasId(nextId - 5)).isEmpty shouldBe false
      sampleGraph.resources.hasId(nextId - 1).orElse(sampleGraph.ns.resources.hasId(nextId - 1)).isEmpty shouldBe false
      sampleGraph.resources.hasId(nextId).isEmpty shouldBe true
    }
  }

  "A new graph" should {
    "be empty" in {
      graph.nodes.count() shouldBe 0
      graph.edges.count() shouldBe 0
      graph.values.count() shouldBe 0
      graph.ns.nodes.count() shouldBe 0
      graph.ns.edges.count() shouldBe 0
      graph.ns.values.count() shouldBe 0
    }
    "be provided with default ontologies, properties and datatypes" in {
      graph.ns.getOntology(Ontology.ontology.iri).isDefined shouldBe true
      graph.ns.getOntology(Property.ontology.iri).isDefined shouldBe true
      graph.ns.getOntology(DataType.ontology.iri).isDefined shouldBe true

      graph.ns.getProperty(Property.default.`@id`.iri).isDefined shouldBe true
      graph.ns.getProperty(Property.default.`@label`.iri).isDefined shouldBe true

      graph.ns.getDataType(DataType.default.`@string`.iri).isDefined shouldBe true
      graph.ns.getDataType(DataType.default.`@geopoint`.iri).isDefined shouldBe true
    }
  }
  "A namespace-graph" can {
    "store and retrieve an ontology" in {
      val unknownOntology = Ontology("unknownOntology")(_extendedClasses = () => List(DataType.ontology))
      graph.ns.getOntology(unknownOntology).isEmpty shouldBe true

      val ontology = graph.ns.storeOntology(unknownOntology)
      ontology.out(Property.default.`@extends`).size shouldBe 1
      ontology.labels.size shouldBe 1
      ontology.iri shouldBe unknownOntology.iri

      graph.ns.getOntology(unknownOntology.iri).isDefined shouldBe true
    }

    "store and retrieve a property" in {}
    "store and retrieve a (custom)-datatype" in {}
  }

  "nodes" can {
    "create a node" which {
      "is empty when no labels are provided" in {
        val node = graph.nodes.create()
        node.out().size shouldBe 0
        node.in().size shouldBe 0
        node.labels.size shouldBe 0
        graph.nodes.hasId(node.id).isDefined shouldBe true
      }
      "have a label when a label is provided" in {
        val node = graph.nodes.create(SampleGraph.ontologies.person)
        node.in().size shouldBe 0
        node.out().size shouldBe 0
        node.hasLabel(SampleGraph.ontologies.person).isDefined shouldBe true
        node.hasLabel(SampleGraph.ontologies.place).isDefined shouldBe false
        graph.nodes.hasId(node.id).isDefined shouldBe true
      }
    }
    "upsert a node by iri" which {
      "creates a new node when no node is identified by this iri" in {
        val node = graph.nodes.upsert("upsert-node-iri")
        node.out().size shouldBe 1
        node.in().size shouldBe 0
        node.labels.size shouldBe 0
        graph.nodes.hasIri("upsert-node-iri").size shouldBe 1
      }
      "returns an existing node when a node is already identified by this iri" in {
        graph.nodes.upsert("upsert-node-iri2")
        val node = graph.nodes.upsert("upsert-node-iri2")
        node.out().size shouldBe 1
        node.in().size shouldBe 0
        node.labels.size shouldBe 0
        graph.nodes.hasIri("upsert-node-iri2").size shouldBe 1
      }
    }
  }

  "edges" should {
    "return a new edge" in {}
  }

  "values" can {
    "create a value" in {
      val value = graph.values.create("unique-word")
      value.value shouldBe "unique-word"
      value.out().size shouldBe 0
      value.in().size shouldBe 0
      value.labels.size shouldBe 1
      graph.values.hasId(value.id).isDefined shouldBe true
      graph.values.byValue("unique-word").size shouldBe 1
    }
    "upsert a value" in {
      graph.values.create("unique-word2")
      val value = graph.values.create("unique-word2")
      value.value shouldBe "unique-word2"
      value.out().size shouldBe 0
      value.in().size shouldBe 0
      value.labels.size shouldBe 1
      graph.values.hasId(value.id).isDefined shouldBe true
      graph.values.byValue("unique-word2").size shouldBe 1
    }
  }

  "ns: NameSpaceGraph" should {
    "contain https://schema.org/Person" in {
      graph.ns.getOntology("https://schema.org/Person").isDefined shouldBe true
    }
  }

  "A graph" should {
    "merge nodes to a single node when upserting an existing iri and multiple nodes are found" in {
      val graph       = createGraph("graphspec-mergeNodes")
      val transaction = graph //.transaction
      1.to(100).map(i => transaction.nodes.create()).map { node =>
        node.addOut(Property.default.typed.createdonDateTime, Instant.now())
        node.addOut(Property.default.typed.iriUrlString, "someuniqueurl")
      }
      transaction.nodes.hasIri("someuniqueurl").size shouldBe 100
      transaction.nodes.upsert("someuniqueurl")
      transaction.nodes.upsert("someuniqueurl")
      transaction.nodes.hasIri("someuniqueurl").size shouldBe 1
//      transaction.commit()
//      Thread.sleep(5000)
//      graph.nodes.hasIri("someuniqueurl").size shouldBe 100
      graph.close()
    }

    "supports transactions" in {
      val graph       = createGraph("graphspec-support-transactions")
      val graphFilled = createSampleGraph("graphspec-support-transactions-filled")
      val transaction = graph.transaction

      graph.nodes.count shouldBe 0
      graph.edges.count shouldBe 0
      graph.values.count shouldBe 0

      graphFilled.nodes.count should not be 0
      graphFilled.edges.count should not be 0
      graphFilled.values.count should not be 0

      SampleGraph.loadSocial(transaction)

      graph.nodes.count shouldBe 0
      graph.edges.count shouldBe 0
      graph.values.count shouldBe 0

      graphFilled.nodes.count shouldBe transaction.nodes.count
      graphFilled.edges.count shouldBe transaction.edges.count
      graphFilled.values.count shouldBe transaction.values.count

      transaction.commit()

      graphFilled.nodes.count shouldBe graph.nodes.count
      graphFilled.edges.count shouldBe graph.edges.count
      graphFilled.values.count shouldBe graph.values.count

      graphFilled.close()
      graph.close()

    }

    "support traversals" which {
      "are detached nodes with traversal-instructions (data)" in {
        val traversal = graph.g
        traversal.self.graph shouldBe DetachedGraph
      }
      "" ignore {
        val node = graph.nodes.upsert("abc")
        node.addLabel(Traversal.ontology)
        val modifiedOn = Instant.now()
        node.addOut(Property.default.typed.modifiedonDateTime, modifiedOn)
        node.addOut(Property.default.`@ids`, "def")
        node.addOut(Property.default.`@ids`, "gef")
        graph.g.N
          .has(Property.default.`@modifiedon`, P.gt(Instant.ofEpochSecond(modifiedOn.getEpochSecond - 100)))
          .count
          .head shouldBe 1
        graph.g.N
          .has(Property.default.`@modifiedon`, P.gt(Instant.ofEpochSecond(modifiedOn.getEpochSecond - 1000)))
          .count
          .head shouldBe 1
        graph.g.N
          .has(Property.default.`@modifiedon`, P.gt(Instant.ofEpochSecond(modifiedOn.getEpochSecond + 100)))
          .count
          .head shouldBe 0
        graph.g.N
          .has(
            Property.default.`@modifiedon`,
            P.gt(Instant.ofEpochSecond(modifiedOn.getEpochSecond - 1000)),
            P.lt(Instant.ofEpochSecond(modifiedOn.getEpochSecond + 1000))
          )
          .count
          .head shouldBe 1
        graph.g.N
          .has(Property.default.`@modifiedon`,
               P.between(Instant.ofEpochSecond(modifiedOn.getEpochSecond - 1000),
                         Instant.ofEpochSecond(modifiedOn.getEpochSecond + 1000)))
          .count
          .head shouldBe 1
        val traversal  = graph.g.N().hasIri("abc").where(_.hasIri("abc")).limit(10).outMap()
        val collection = Collection(Instant.now(), Instant.now(), traversal.toList)(traversal.ct)
        collection.item.head.nonEmpty shouldBe true
      }
    }

    "support inserting structures from other graphs (object + edges)" ignore {
      val traversal    = graph.ns.g.N().out(Property.default.`@id`).out(Property.default.`@language`)
      val node         = traversal.self
      val upsertedNode = graph.nodes.post(node)
      //      graph.ldParser.toJSON.nodeToJsonWithContext(node)._1.toString shouldBe graph.ldParser.toJSON.nodeToJsonWithContext(upsertedNode.asInstanceOf[Node])._1.toString
      //      node.property(graph.idUrlString, "abc")
      graph.nodes.upsert(node.iri)
    }
  }

  "Graphs" can {
    "be merged" in {
      val newGraph = createGraph("graphspec2")

      newGraph.nodes.count shouldBe 0
      newGraph.edges.count shouldBe 0
      newGraph.values.count shouldBe 0

      newGraph ++ sampleGraph

//      println(newGraph.g.N.toList.map(_.iri))
//      println(newGraph.g.N.toList.map(_.id))
//      println(graph.g.N.toList.map(_.iri))
//      println(graph.g.N.toList.map(_.id))
      newGraph.nodes.count shouldBe sampleGraph.nodes.count
      newGraph.edges.count shouldBe sampleGraph.edges.count
      newGraph.values.count shouldBe sampleGraph.values.count

      newGraph.close()
    }
  }
}
