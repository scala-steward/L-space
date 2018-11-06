package lspace.librarian.structure

import java.time.Instant

import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.step.V
import lspace.NS.types
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.util.SampleGraph
import lspace.types._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

trait GraphSpec extends WordSpec with Matchers with BeforeAndAfterAll {
  def graph: Graph
  def createGraph(iri: String): Graph
  def createDefaultGraph(iri: String): Graph = {
    val graph = createGraph(iri)
    SampleGraph.loadSocial(graph)
    graph
  }

  override def beforeAll = {
    SampleGraph.loadSocial(graph)
    println(MemGraphDefault.ns.ontologies.keySet)
    println(MemGraphDefault.ns.properties.keySet)
    println(MemGraphDefault.ns.datatypes.keySet)
  }

  "A Graph" should {
    "have the jsonld default ontologies" in {

      val ontology = graph.ns.storeOntology(V.ontology)
      List(ontology, ontology, ontology).toSet.size shouldBe 1
      List(ontology, ontology, ontology).map(Ontology.apply).toSet.size shouldBe 1
      ontology.out(Property.default.EXTENDS).size shouldBe 1
      ontology.labels.size shouldBe 1
      graph.ns.g.N().hasIri(V.ontology.iri).toList.nonEmpty shouldBe true
      graph.ns.g.N().hasIri(V.ontology.iri).toList.head.iri shouldBe V.ontology.iri
      graph.ns.getOntology(types.CLASS).isDefined shouldBe true
      V.ontology.extendedClasses.size shouldBe 1
      ontology.out(Property.default.EXTENDS).size shouldBe 1
    }
    "store an ontology and its properties and return the persisted ontology as detached" in {
      val ontology = graph.ns.storeOntology(EqP.ontology)
      ontology.out(Property.default.properties).size shouldBe 1
      val eqOntology = Ontology(graph.ns.storeOntology(p.Eqv.ontology))
      eqOntology.`extends`(P.ontology) shouldBe true
      eqOntology.`extends`(EqP.ontology) shouldBe true
      eqOntology.properties.size shouldBe 1
    }
    "test get 3 iri" in {
      val iris = graph.ns.g.N().out(Property.default.iri).range(2, 5).toList
      iris.size shouldBe 3
      val iris2 = graph.ns.g.N().out(Property.default.iri).limit(3).toList
      iris2.size shouldBe 3
      val size = graph.ns.g.N().out(Property.default.iri).limit(3).count.head
      size shouldBe 3
    }
    "upsert structures from other graphs (e.g. DetachedGraph)" ignore {
      val traversal    = graph.ns.g.N().out(Property.default.iri).out(Property.default.language)
      val node         = traversal.self
      val upsertedNode = graph.postNode(node)
      //      graph.ldParser.toJSON.nodeToJsonWithContext(node)._1.toString shouldBe graph.ldParser.toJSON.nodeToJsonWithContext(upsertedNode.asInstanceOf[Node])._1.toString
      //      node.property(graph.idUrlString, "abc")
      graph.upsertNode(node.iri)
    }
  }

  "A traversal" can {
    "start with a ResourceStep" in {
      graph.ns.g.N().hasLabel(Ontology.ontology).headOption.isDefined shouldBe true
      val property = graph.ns.getNode(V.ontology).head --- Property.default.language --> "abc"
      graph.ns.g.E().hasLabel(Property.default.label).headOption.isDefined shouldBe true
      graph.ns.g.V.hasLabel(DataType.default.textType).headOption.isDefined shouldBe true
    }
    "start without a ResourceStep" in {
      graph.ns.g.N.hasLabel(Ontology.ontology).headOption.isDefined shouldBe true
      graph.ns.g.E.hasLabel(Property.default.label).headOption.isDefined shouldBe true
      graph.ns.g.V.hasLabel(DataType.default.textType).headOption.isDefined shouldBe true
    }
    "have valid paths for" in {
      //      graph.ontology.out(MemGraphDefault.typeDatatype).take(1).map(DataType.toDataType).head.asInstanceOf[LiteralType[T]]
      //      graph.g.N(Ontology.ontology).toList.nonEmpty shouldBe true
      //      graph.g.N(Ontology.ontology).head.iri shouldBe Ontology.ontology.iri
      //      graph.g.N.has(graph.id, P.eqv(Ontology.ontology.iri)).toList.size shouldBe 1
      //      graph.g.N.has(graph.id, P.eqv(Ontology.ontology.iri)).head.iri shouldBe Ontology.ontology.iri
      //      graph.g.N(Ontology.ontology).out().toList.nonEmpty shouldBe true
      //      graph.g.N(Ontology.ontology).out(graph.TYPE).toList.nonEmpty shouldBe true
      //      graph.g.N().hasId(Ontology.ontology.iri).out(graph.id).toList.size shouldBe 1
      //      graph.g.N.hasLabel(Ontology.ontology).limit(10).toList.size shouldBe 10
      //      graph.g.N.hasLabel(Ontology.ontology)
      //        .where(_.label().has(graph.id, P.eqv(Ontology.ontology.iri))).limit(10).outMap()
      //        .toList.size shouldBe 10
      //      graph.g.N().hasLabel(Ontology.ontology).outMap().toList.nonEmpty shouldBe true
      //      graph.g.N().or(_.hasId(Ontology.ontology.iri), _.hasId(graph.label.iri)).out(graph.id).toList.size shouldBe 2
      //      graph.g.N().or(_.hasId(Ontology.ontology.iri), _.hasId(graph.label.iri)).out(graph.id).outMap().toList.size shouldBe 2
      //      graph.g.N().or(_.hasId(Ontology.ontology.iri), _.hasId(graph.label.iri)).out(graph.id).toList should contain allOf (Ontology.ontology.iri, graph.label.iri)
      //      graph.g.N().or(_.hasId("notexistingiri3fi82094jlsdzvz94z8943as"), _.hasId(graph.label.iri)).out(graph.id).toList should contain(graph.label.iri)
      //      graph.g.N().or(_.hasId("notexistingiri3fi82094jlsdzvz94z8943as"), _.hasId(graph.label.iri)).out(graph.id).toList.size shouldBe 1
      //      graph.g.N().and(_.hasId(Ontology.ontology.iri), _.hasId(graph.label.iri)).out(graph.id).toList.isEmpty shouldBe true
      //      //      graph.g.N(Ontology.ontology).outE(graph.properties).toList.map(_.inV.iri) shouldBe 1
      //      graph.g.N(Ontology.ontology).has(graph.properties, P.eqv(graph.label.value)).toList.size shouldBe 1
      //      graph.g.N(Ontology.ontology).has(graph.properties, P.eqv(graph.base.value)).toList.size shouldBe 0
      //      graph.g.N(Ontology.ontology).has(graph.properties, P.eqv(graph.label.value)).has(graph.properties, P.eqv(graph.comment.value)).out(graph.id).toList.size shouldBe 1
      //      graph.g.N(Ontology.ontology).or(_.has(graph.properties, P.eqv(graph.label.value)), _.has(graph.properties, P.eqv(graph.base.value))).out(graph.id).toList.size shouldBe 1
      //      graph.g.N(Ontology.ontology).and(_.has(graph.properties, P.eqv(graph.label.value)), _.has(graph.properties, P.eqv(graph.base.value))).out(graph.id).toList.size shouldBe 0
      //      graph.g.N(Ontology.ontology).and(_.has(graph.properties, P.eqv(graph.label.value)), _.has(graph.properties, P.eqv(graph.comment.value))).out(graph.id).toList.size shouldBe 1
      //      graph.g.N(Ontology.ontology).out().toList.contains(graph.ontology) shouldBe true //Ontology has TYPE Ontology, so it must be a value.
      //      graph.g.N(Ontology.ontology).outE().toList.nonEmpty shouldBe true
      //      graph.g.N(Ontology.ontology).outE().toList.exists(_.key == graph.id) shouldBe true
      //      graph.g.N(Ontology.ontology).outMap(graph.id).toList.nonEmpty shouldBe true
      //      graph.g.N(Ontology.ontology).outMap(graph.id).head.contains(graph.id) shouldBe true
      //
      //      graph.g.N(Ontology.ontology).outMap().toList.nonEmpty shouldBe true
      //      graph.g.N(Ontology.ontology).outMap().head.contains(graph.TYPE) shouldBe true
      //      //      graph.g.N(Ontology.ontology).outMap().head(graph.TYPE).contains(graph.ontology) shouldBe true
      //      graph.g.N(Ontology.ontology).group(_.label()).outE().head.head._2.map(_.inV).contains(Ontology.ontology) shouldBe false
      //      graph.g.N(Ontology.ontology).group(_.label()).outE().head.head._2.map(_.inV.iri).contains(Ontology.ontology.iri) shouldBe true
      //
      //      graph.g.N(Ontology.ontology).inMap().toList.nonEmpty shouldBe true
      //      graph.g.N(Ontology.ontology).inMap().head.contains(graph.TYPE) shouldBe true
      //      graph.g.N(Ontology.ontology).inMap().head.contains(graph.EXTENDS) shouldBe true
      //
      //      graph.g.N(Ontology.ontology).inEMap().toList.nonEmpty shouldBe true
      //      graph.g.N(Ontology.ontology).inEMap().head.contains(graph.TYPE) shouldBe true
      //      graph.g.N(Ontology.ontology).inEMap().head.contains(graph.EXTENDS) shouldBe true
      //
      //      graph.g.N(Ontology.ontology).out().toList.size should be > 2
      //      graph.g.N(Ontology.ontology).out().limit(1).toList.size shouldBe 1
      //      graph.g.N(Ontology.ontology).out().range(2, 3).toList.size shouldBe 1
      //      graph.g.N(Ontology.ontology).hasId(Ontology.ontology.iri).toList.size shouldBe 1
      //      graph.g.N(Ontology.ontology).hasLabel(Ontology.ontology).toList.size shouldBe 1
      //      graph.g.N(Ontology.ontology).has(graph.TYPE).toList.size shouldBe 1
      //      graph.g.N().has(graph.TYPE).count.toList.size shouldBe 1
      //      graph.g.N().hasLabel(Ontology.ontology).count.toList.size shouldBe 1
      //      graph.g.N().hasLabel(Ontology.ontology).count.toList.size shouldBe 1
      //      graph.g.N().hasLabel(Ontology.ontology).count.head should be >= 10l
      //      graph.g.N().has(graph.id, P.eqv(Ontology.ontology.iri)).count.head should be >= 1l
      //      graph.g.N().has(graph.id).count.head should be >= 10l
      //      graph.g.N().label().count.head should be >= 10l

      //      graph.g.N(Ontology.ontology).has(graph.typeOntology, P.eqs(Ontology.ontology.value)).toList.size shouldBe 1
      //      graph.g.N(Ontology.ontology).has(Property.default.id, P.eqv(Ontology.ontology.iri)).toList.size shouldBe 1
      //      graph.g.N(Ontology.ontology).has(Property.default.id, P.eqv(Ontology.ontology.iri)).toList.size shouldBe 1
      //      graph.g.N(Ontology.ontology).has(Property.default.TYPE, P.eqv(Ontology.ontology)).toList.size shouldBe 1
      //      graph.g.N(Ontology.ontology).union(_.out(graph.idUrlString), _.out(graph.typeOntology)).toList.size shouldBe 2

    }
    "mergeNodes should merge supplied nodes to a single object" in {
      val graph = createDefaultGraph("graphspec-mergeNodes")
      1.to(100).map(i => graph.createNode()).map { node =>
        node.addOut(Property.default.typed.createdonDateTime, Instant.now())
        node.addOut(Property.default.typed.iriUrlString, "someuniqueurl")
      }
      graph.getNode("someuniqueurl").size shouldBe 100
      graph.upsertNode("someuniqueurl")
      graph.upsertNode("someuniqueurl")
      graph.getNode("someuniqueurl").size shouldBe 1
    }

    "parse OutMap to objects in json" in {
      val node = graph.upsertNode("abc")
      node.addLabel(Traversal.ontology)
      val modifiedOn = Instant.now()
      node.addOut(Property.default.typed.modifiedonDateTime, modifiedOn)
      node.addOut(Property.default.iris, "def")
      node.addOut(Property.default.iris, "gef")
      graph.g.N
        .has(Property.default.modifiedon, P.gt(Instant.ofEpochSecond(modifiedOn.getEpochSecond - 100)))
        .count
        .head shouldBe 1
      graph.g.N
        .has(Property.default.modifiedon, P.gt(Instant.ofEpochSecond(modifiedOn.getEpochSecond - 1000)))
        .count
        .head shouldBe 1
      graph.g.N
        .has(Property.default.modifiedon, P.gt(Instant.ofEpochSecond(modifiedOn.getEpochSecond + 100)))
        .count
        .head shouldBe 0
      graph.g.N
        .has(Property.default.modifiedon,
             P.gt(Instant.ofEpochSecond(modifiedOn.getEpochSecond - 1000)),
             P.lt(Instant.ofEpochSecond(modifiedOn.getEpochSecond + 1000)))
        .count
        .head shouldBe 1
      graph.g.N
        .has(Property.default.modifiedon,
             P.between(Instant.ofEpochSecond(modifiedOn.getEpochSecond - 1000),
                       Instant.ofEpochSecond(modifiedOn.getEpochSecond + 1000)))
        .count
        .head shouldBe 1
      val traversal  = graph.g.N().hasIri("abc").where(_.has(Property.default.iri, P.eqv("abc"))).limit(10).outMap()
      val collection = Collection(Instant.now(), Instant.now(), traversal.toList)(traversal.ct)
      collection.item.head.nonEmpty shouldBe true
    }

    "A local-step" should {
      "traverse in local scope" in {
        //        graph.g.N(Ontology.ontology).local(_.out(graph.TYPE).count).head shouldBe 1
        //        graph.g.N(Ontology.ontology).where(_.out(graph.TYPE).count).head shouldBe 1
        //        println(graph.g.N.where(_.out(graph.TYPE).count.is(P.eqv(1))).limit(100).toList)
        //        graph.g.N(Ontology.ontology).where(_.out(graph.TYPE).count.is(P.gt(0))).toList.size shouldBe 1
        //        graph.g.N(Ontology.ontology).union(_.has(graph.TYPE, P.eqv(Ontology.ontology.value)), _.has(graph.TYPE, P.eqv(Ontology.ontology.value))).toList.size shouldBe 2
      }
    }
  }

  "Graphs" can {
    "be merged" in {
      val graph    = createDefaultGraph("graphspec-graphs-be-merged")
      val newGraph = createGraph("graphspec2")

      newGraph.g.N.count.head shouldBe 0
      newGraph.g.E.count.head shouldBe 0
      newGraph.g.V.count.head shouldBe 0

      newGraph ++ graph

      newGraph.g.N.count.head shouldBe graph.g.N.count.head
      newGraph.g.E.count.head shouldBe graph.g.E.count.head
      newGraph.g.V.count.head shouldBe graph.g.V.count.head

      newGraph.g.N.has("name", P.eqv("Garrison")).outMap().head.size shouldBe 5
    }
  }
}
