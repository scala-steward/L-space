package lspace.librarian.structure

import java.time.Instant

import lspace.librarian.datatype.DataType
import lspace.librarian.process.traversal._
import lspace.librarian.structure.Property.default._
import lspace.librarian.util.SampleGraph
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraph
import monix.eval.Task
import monix.reactive.Observable
import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, Matchers, Outcome}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.collection.JavaConverters._

trait GraphSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll with GraphFixtures {
  import SampleGraph.ontologies._
  import SampleGraph.properties._

  import monix.execution.Scheduler.Implicits.global

//  def take = afterWord("take")

  def graphTests(graph: Graph) = {
    "a graph" should {
      "have an id provider" which {
        "provides unique a unique id, even when dealing with concurrency" in {
          Task
            .gather((1 to 100).map(i => Task.now(graph.idProvider.next)))
            .map { ids =>
              ids.toSet.size shouldBe 100
            }
            .runToFuture
        }
      }
      "have a nodes API" which {
        "can create a node" which {
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
            graph.nodes.hasId(node.id).size shouldBe 1
            graph.g.N.hasId(node.id).toList.size shouldBe 1
            graph.values.byValue("upsert-node-iri").size shouldBe 1
            graph.values.byValue("upsert-node-iri").flatMap(_.in()).size shouldBe 1
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
        "merges existing nodes when multiple nodes in the graph are identified by the same iri" in {
          import Property.default._
          graph.nodes.upsert("dup-existing-node-123")
          graph.nodes.hasIri("dup-existing-node-123").size shouldBe 1
          graph.nodes.create() --- `@id` --> "dup-existing-node-123"
          graph.nodes.create() --- `@id` --> "dup-existing-node-123"
          graph.nodes.create() --- `@id` --> "dup-existing-node-123"
          graph.nodes.upsert("dup-existing-node-123")
          Task
            .defer { //merging nodes is a async side-effect of upsert, the delay should be enough so that the previous mergetask can finish
              graph.nodes.hasIri("dup-existing-node-123").size shouldBe 1
              graph.nodes.hasIri("dup-existing-node-123").size shouldBe 1
              graph.nodes.create() --- `@id` --> "dup-existing-node-123"
              graph.nodes.create() --- `@id` --> "dup-existing-node-123"
              graph.nodes.create() --- `@id` --> "dup-existing-node-123"
              graph.nodes.upsert("dup-existing-node-123")
              Task { //merging nodes is a async side-effect of upsert, the delay should be enough so that the previous mergetask can finish
                graph.nodes.hasIri("dup-existing-node-123").size shouldBe 1
              }.delayExecution(300.millis)
            }
            .delayExecution(300.millis)
            .runToFuture(monix.execution.Scheduler.global)
        }
      }
      "have an edges API" which {}
      "have a values API" which {
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
      "have a resources API" which {}

      "merge nodes to a single node when upserting an existing iri and multiple nodes are found" in {
        //      val graph = createGraph("graphspec-mergeNodes")
        try {
          1.to(10).map(i => graph.nodes.create()).map { node =>
            node.addOut(Property.default.typed.createdonDateTime, Instant.now())
            node.addOut(Property.default.typed.iriUrlString, "someuniqueurl")
          }
          val transaction = graph.transaction
          1.to(90).map(i => transaction.nodes.create()).map { node =>
            node.addOut(Property.default.typed.createdonDateTime, Instant.now())
            node.addOut(Property.default.typed.iriUrlString, "someuniqueurl")
          }
          transaction.nodes.hasIri("someuniqueurl").size shouldBe 100
          transaction.nodes.upsert("someuniqueurl")
          //      transaction.nodes.upsert("someuniqueurl")
          transaction.nodes.hasIri("someuniqueurl").size shouldBe 1
          transaction.commit()
          graph.nodes.hasIri("someuniqueurl").size shouldBe 1
        } catch {
          case t: Throwable =>
            //          t.printStackTrace()
            fail(t.getMessage)
        } finally {
          graph.close()
        }
      }

      "support traversals" which {
        "are detached nodes with traversal-instructions (data)" in {
          val traversal = graph.g
          traversal.target shouldBe graph
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
              P.gt(Instant.ofEpochSecond(modifiedOn.getEpochSecond - 1000)) &&
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
          val collection = Collection(Instant.now(), Instant.now(), traversal.toList, traversal.ct)
          collection.item.head.nonEmpty shouldBe true
        }
      }

    }
  }
  def sampledGraphTests(sampledGraph: SampledGraph) = {
    val sampleGraph = sampledGraph.graph

    "a samplegraph graph" should {
      "have a namespace" which {
        "contains the person-ontology" in {
          sampleGraph.ns.ontologies
            .get(SampleGraph.Person.iri)
            .map { ontologyOption =>
              ontologyOption shouldBe Some(SampleGraph.Person.ontology)
            }
            .runToFuture
        }
        "contains the person-ontology in cache" in {
          sampleGraph.ns.ontologies.cached(SampleGraph.Person.iri) shouldBe Some(SampleGraph.Person.ontology)
        }
        "contains the person-ontology in the global cache" in {
          Ontology.ontologies.cached(SampleGraph.Person.iri) shouldBe Some(SampleGraph.Person.ontology)
        }
        "contains the place-ontology" in {
          sampleGraph.ns.ontologies
            .get(SampleGraph.Place.iri)
            .map { ontologyOption =>
              ontologyOption shouldBe Some(SampleGraph.Place.ontology)
            }
            .runToFuture
        }
        "contains the place-ontology in cache" in {
          sampleGraph.ns.ontologies.cached(SampleGraph.Place.iri) shouldBe Some(SampleGraph.Place.ontology)
        }
        "contains the place-ontology in the global cache" in {
          Ontology.ontologies.cached(SampleGraph.Place.iri) shouldBe Some(SampleGraph.Place.ontology)
        }
        "contains the name-property" in {
          sampleGraph.ns.properties
            .get(SampleGraph.properties.name.iri)
            .map { propertyOption =>
              propertyOption shouldBe Some(SampleGraph.properties.name.property)
            }
            .runToFuture
        }
        "contains the name-property in cache" in {
          sampleGraph.ns.properties.cached(SampleGraph.properties.name.iri) shouldBe Some(
            SampleGraph.properties.name.property)
        }
        "contains the name-property in the global cache" in {
          Property.properties.cached(SampleGraph.properties.name.iri) shouldBe Some(
            SampleGraph.properties.name.property)
        }
      }
      "have sample data" which {
        "contains certain nodes" in {
          val Garrison = sampleGraph.nodes.hasIri(sampleGraph.iri + "/person/56789").headOption
          Garrison.map(_.labels) shouldBe Some(List(person))
          sampleGraph.nodes.count() shouldBe 10
        }
        "contains certain edges" in {
          val Yoshio = sampleGraph.nodes.hasIri(sampleGraph.iri + "/person/123").headOption
          sampleGraph.edges().find(e => e.key == name.property).map(_.key) shouldBe Some(name.property)

          sampleGraph.edges.count() shouldBe 58
        }
        "contains certain values" in {

          sampleGraph.values.count() shouldBe 38
        }
      }
      //        "support inserting structures from other graphs (object + edges)" ignore {
      //          val traversal    = graph.ns.g.N().out(Property.default.`@id`).out(Property.default.`@language`)
      //          val node         = traversal.toNode
      //          val upsertedNode = graph.nodes.post(node)
      //          //      graph.ldParser.toJSON.nodeToJsonWithContext(node)._1.toString shouldBe graph.ldParser.toJSON.nodeToJsonWithContext(upsertedNode.asInstanceOf[Node])._1.toString
      //          //      node.property(graph.idUrlString, "abc")
      //          graph.nodes.upsert(node.iri)
      //        }
      "be able to merge" in {
        val newGraph = MemGraph("graphspec2merge")

        newGraph.nodes().size shouldBe 0
        newGraph.edges().size shouldBe 0
        newGraph.values().size shouldBe 0

        newGraph ++ sampleGraph

        newGraph.close()

        newGraph.nodes().size shouldBe sampleGraph.nodes.count
        newGraph.edges().size shouldBe sampleGraph.edges.count
        newGraph.values().size shouldBe sampleGraph.values.count
      }
    }
  }

  def benchmarkTests(graph: Graph) = {
    "a graph" can {
      "create nodes in parallel" ignore { //more of a benchmark, enable to test concurrency
        val newIds: scala.collection.concurrent.Map[Long, List[Long]] =
          new java.util.concurrent.ConcurrentHashMap[Long, List[Long]]().asScala

        val start = java.time.Instant.now().toEpochMilli
        Future {
          (1 to 500).map { i =>
            val node = graph.nodes.upsert(s"some-iri-1,000-1-$i")
            node
          }
        }.flatMap { t =>
          Future
            .sequence(
              1.to(5)
                .flatMap(i =>
                  Seq(
                    Future {
                      val transaction1 = graph.transaction
                      val nodes = (1 to 1000).map { i =>
                        val node = transaction1.nodes.upsert(s"some-iri-1,000-1-$i")
                        node
                      }
                      transaction1
                        .commit()
                    },
                    Future {
                      val transaction2 = graph.transaction
                      val nodes = (1 to 1000).map { i =>
                        val node = transaction2.nodes.create()
                        node --- `@id` --> s"some-iri-1,000-2-$i"
                        //                      newIdsLock.synchronized {
                        //                        newIds += 0l -> (node.id :: newIds.getOrElse(0l, List()))
                        //                      }
                        node
                      }
                      transaction2.nodes.added.map(_._2.iri).count(_.startsWith("some-iri-1,000-2-")) shouldBe 1000
                      transaction2.nodes.added.size shouldBe 1000
                      transaction2.edges.added.size shouldBe 1000
                      transaction2
                        .commit()
                    }
                )))
            .map { r =>
              val end      = java.time.Instant.now().toEpochMilli
              val duration = end - start

              println(s"create 5x 1,000 nodes in parallel took ${duration} milli-seconds")
              //            println(s"total nodes: ${graph.nodes.count()}")
              //            println(s"total nodes: ${graph.nodes().size}")
              //            println(s"total values: ${graph.values.count()}")
              //            println(s"total values: ${graph.values().size}")
              //            println(graph.nodes().size)
              //            println(s"ids: ${newIds.head._2.size} :: ${newIds.head._2.toSet.size}")
              //            println(newIds.head._2.groupBy(identity).collect { case (x, List(_, _, _*)) => x })
              //            graph.nodes().flatMap(_.iris.headOption).count(_.startsWith("some-iri-1,000-2-")) shouldBe 5000
              //            println(s"empty iris ${graph.nodes().count(_.iri.isEmpty)}")
              graph.nodes().map(_.iri).count(_.startsWith("some-iri-1,000-2-")) shouldBe 5000
              graph.g.N.has(`@id`, P.prefix("some-iri-1,000-2")).count().head shouldBe 5000l
              graph.g.N.has(`@id`, P.prefix("some-iri-1,000-1")).count().head should (be >= 1000l and be < 6000l)
            }
        }
      }

      "create edges in parallel" ignore { //more of a benchmark, enable to test concurrency
        Observable
          .fromIterable(1 to 5000)
          .mapParallelUnordered(20)(i => Task(graph.nodes.upsert("abcabc") --- `@id` --> graph.nodes.upsert("defdef")))
          .completedL
          .runToFuture(monix.execution.Scheduler.global)
          .map { t =>
            1 shouldBe 1
          }
      }

//      "contain all base ontologies and properties" in {
//        //      MemGraphDefault.ns.ontologies.byIri.size should be > 0
//        //      MemGraphDefault.ns.properties.byIri.size should be > 0
//      }
      "get 10,000 times from index" ignore {
        val start   = java.time.Instant.now().toEpochMilli
        val newNode = graph.nodes.upsert("10000-unique-iri")
        val id      = graph.nodes.hasIri("10000-unique-iri").head.id
        (1 to 10000).foreach(_ => graph.nodes.hasId(id))
        val end      = java.time.Instant.now().toEpochMilli
        val duration = end - start
        scribe.info(s"get 10,000 times from index took ${duration} milli-seconds")
        1 shouldBe 1
      }
      "create 10,000 nodes with an iri" ignore {
        val start       = java.time.Instant.now().toEpochMilli
        val transaction = graph.transaction
        (1 to 10000).foreach { i =>
          val node = transaction.nodes.create()
          node --- `@id` --> s"some-iri-10,000-$i"
        }
        transaction
          .commit()

        val end      = java.time.Instant.now().toEpochMilli
        val duration = end - start
        scribe.info(s"create 10,000 nodes took ${duration} milli-seconds")
        1 shouldBe 1
      }
      "upsert 10,000 nodes with a unique iri" ignore {
        val start       = java.time.Instant.now().toEpochMilli
        val transaction = graph.transaction
        (1 to 10000).foreach { i =>
          val node = transaction.nodes.upsert(s"some-upsert-iri-10,000-$i")
        }
        transaction
          .commit()

        val end      = java.time.Instant.now().toEpochMilli
        val duration = end - start
        scribe.info(s"upsert 10,000 nodes took ${duration} milli-seconds")
        1 shouldBe 1
      }
      "create 20,000 nodes with an iri" ignore {
        val start       = java.time.Instant.now().toEpochMilli
        val transaction = graph.transaction
        (1 to 20000).foreach { i =>
          val node = transaction.nodes.create()
          node --- `@id` --> s"some-iri-2,000-$i"
        }
        transaction
          .commit()

        val end      = java.time.Instant.now().toEpochMilli
        val duration = end - start
        scribe.info(s"create 20,000 nodes took ${duration} milli-seconds")
        1 shouldBe 1
      }

      //    "upsert 40,000 different uri's" in {
      //      val mb = 1024 * 1024
      //      val runtime = Runtime.getRuntime
      //      println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
      //      println("** Free Memory:  " + runtime.freeMemory / mb)
      //      println("** Total Memory: " + runtime.totalMemory / mb)
      //      println("** Max Memory:   " + runtime.maxMemory / mb)
      //
      //      val start = java.time.Instant.now()
      //      println(start)
      //      //      val x = (1 to 80000).map(i => graph.nodes.upsert(s"https://some.example.com/$i"))
      //      //      val x = (1 to 80000).map(i => graph.newValue(s"https://some.example.com/$i"))
      //      val x = (1 to 80000).map(i => graph.newNode()).map { n => n.property(graph.TYPE, "a"); n }
      //      //      println("#nodes: " + graph.nodes.size)
      //      //      println("#edges: " + graph.links.size)
      //      //      println("#values: " + graph.values.size)
      //      val end = java.time.Instant.now()
      //      println(end)
      //      val duration = end.getEpochSecond - start.getEpochSecond
      //      println(s"upsert 10,000 different uri's took ${duration} seconds")
      //
      //      println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
      //      println("** Free Memory:  " + runtime.freeMemory / mb)
      //      println("** Total Memory: " + runtime.totalMemory / mb)
      //      println("** Max Memory:   " + runtime.maxMemory / mb)
      //      println(x.size)
      //    }
    }
  }
}
