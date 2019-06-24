package lspace.structure

import java.time.Instant

import lspace._
import lspace.librarian.task.Guide
import lspace.provider.mem.MemGraph
import lspace.util.SampleGraph
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, Matchers, Outcome}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.collection.JavaConverters._

trait GraphSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll with GraphFixtures {
  import SampleGraph.ontologies._
  import SampleGraph.properties._

  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global
  implicit def guide: Guide[Observable]

//  def take = afterWord("take")

  def graphTests(graph: Graph) = {
    "a graph" should {
      "have an id provider" which {
        "provides unique a unique id, even when dealing with concurrency" in {
          Task
            .gather((1 to 100).map(i => graph.idProvider.next))
            .map { ids =>
              ids.toSet.size shouldBe 100
            }
            .timeout(400.millis)
            .runToFuture
        }
      }
      "have a nodes API" which {
        "can create a node" which {
          "is empty when no labels are provided" in {
            (for {
              node <- graph.nodes.create()
            } yield {
              node.out().size shouldBe 0
              node.in().size shouldBe 0
              node.labels.size shouldBe 0
//              graph.nodes.hasId(node.id).isDefined shouldBe true
            }).timeout(400.millis).runToFuture
          }
          "have a label when a label is provided" in {
            (for {
              node <- graph.nodes.create(SampleGraph.ontologies.person)
            } yield {
              node.in().size shouldBe 0
              node.out().size shouldBe 0
              node.hasLabel(SampleGraph.ontologies.person).isDefined shouldBe true
              node.hasLabel(SampleGraph.ontologies.place).isDefined shouldBe false
//              graph.nodes.hasId(node.id).isDefined shouldBe true
            }).timeout(400.millis).runToFuture
          }
        }
        "upsert a node by iri" which {
          "creates a new node when no node is identified by this iri" in {
            (for {
              node <- graph.nodes.upsert("upsert-node-iri")
              _    <- graph.nodes.hasId(node.id).map(_.size shouldBe 1)
              //            graph.g.N.hasId(node.id).toList.size shouldBe 1
              _ <- graph.values.byValue("upsert-node-iri").toListL.map(_.size shouldBe 1)
              _ <- graph.values
                .byValue("upsert-node-iri")
                .map(_.in())
                .flatMap(Observable.fromIterable)
                .toListL
                .map(_.size shouldBe 1)
              _ <- graph.nodes.hasIri("upsert-node-iri").toListL.map(_.size shouldBe 1)
            } yield {
              node.out().size shouldBe 1
              node.in().size shouldBe 0
              node.labels.size shouldBe 0
            }).timeout(400.millis).runToFuture
          }
          "returns an existing node when a node is already identified by this iri" in {
            (for {
              node  <- graph.nodes.upsert("upsert-node-iri2")
              node2 <- graph.nodes.upsert("upsert-node-iri2")
              _ = node.id shouldBe node2.id
//              _ = Thread.sleep(500)
              _ = node.out().size shouldBe 1
              _ <- graph.nodes.hasIri("upsert-node-iri2").toListL.map(_.size shouldBe 1)
            } yield {
//              println(node.out())
//              println(node.outE().map(_.prettyPrint))
              node.out().size shouldBe 1
              node.in().size shouldBe 0
              node.labels.size shouldBe 0
            }).timeout(400.millis).runToFuture
          }
        }
        "merges existing nodes when multiple nodes in the graph are identified by the same iri" in {
          import Label.P._
          (for {
            _ <- graph.nodes.upsert("dup-existing-node-123")
            _ <- graph.nodes.hasIri("dup-existing-node-123").toListL.map(_.size shouldBe 1)
            _ <- graph.nodes.create().flatMap(_ --- `@id` --> "dup-existing-node-123")
            _ <- graph.nodes.create().flatMap(_ --- `@id` --> "dup-existing-node-123")
            _ <- graph.nodes.create().flatMap(_ --- `@id` --> "dup-existing-node-123")
            _ <- graph.nodes.upsert("dup-existing-node-123")
            _ <- (for { //merging nodes is a async side-effect of upsert, the delay should be enough so that the previous mergetask can finish
              _ <- graph.nodes.hasIri("dup-existing-node-123").toListL.map(_.size shouldBe 1)
              _ <- graph.nodes.hasIri("dup-existing-node-123").toListL.map(_.size shouldBe 1)
              _ <- graph.nodes.create().flatMap(_ --- `@id` --> "dup-existing-node-123")
              _ <- graph.nodes.create().flatMap(_ --- `@id` --> "dup-existing-node-123")
              _ <- graph.nodes.create().flatMap(_ --- `@id` --> "dup-existing-node-123")
              _ <- graph.nodes.upsert("dup-existing-node-123")
              _ <- //merging nodes is a async side-effect of upsert, the delay should be enough so that the previous mergetask can finish
              graph.nodes.hasIri("dup-existing-node-123").toListL.map(_.size shouldBe 1) //.delayExecution(200.millis)
            } yield ()) //.delayExecution(200.millis)
          } yield { succeed }).timeout(800.millis).runToFuture
        }
      }
//      "have an edges API" which {}
      "have a values API" which {
        "create a value" in {
          (for {
            value <- graph.values.create("unique-word")
            _     <- graph.values.hasId(value.id).map(_.isDefined shouldBe true)
            _     <- graph.values.byValue("unique-word").toListL.map(_.size shouldBe 1)
          } yield {
            value.value shouldBe "unique-word"
            value.out().size shouldBe 0
            value.in().size shouldBe 0
            value.labels.size shouldBe 1
          }).timeout(400.millis).runToFuture
        }
        "upsert a value" in {
          (for {
            _     <- graph.values.create("unique-word2")
            value <- graph.values.create("unique-word2")
            _     <- graph.values.hasId(value.id).map(_.isDefined shouldBe true)
            _     <- graph.values.byValue("unique-word2").toListL.map(_.size shouldBe 1)
          } yield {
            value.value shouldBe "unique-word2"
            value.out().size shouldBe 0
            value.in().size shouldBe 0
            value.labels.size shouldBe 1
          }).timeout(400.millis).runToFuture
        }
      }
//      "have a resources API" which {}

      "merge nodes to a single node when upserting an existing iri and multiple nodes are found" in {
        val transaction = graph.transaction
        (for {
          nodes <- Task.sequence {
            1.to(100).map { i =>
              for {
                node <- transaction.nodes.create().onErrorHandle { f =>
                  println(f.getMessage); throw f
                }
//                _    <- node.addOut(Label.P.typed.createdonDateTime, Instant.now())
                edge <- node.addOut(Label.P.typed.iriUrlString, "someuniqueurl").onErrorHandle { f =>
                  println(f.getMessage); throw f
                }
              } yield node
            }
          }
          _ <- transaction.nodes.hasIri("someuniqueurl").toListL.map(_.size shouldBe 100)
          _ <- transaction.nodes.upsert("someuniqueurl")
          _ <- transaction.nodes.hasIri("someuniqueurl").toListL.map(_.size shouldBe 1)
          _ <- graph.nodes.hasIri("someuniqueurl").toListL.map(_.size shouldBe 0)
          c <- transaction.commit()
          _ <- graph.nodes.hasIri("someuniqueurl").toListL.map(_.size shouldBe 1)
        } yield succeed).timeout(1200.millis).runToFuture
      }

      "support traversals" which {
        "test..." in {
          (for {
            node <- graph.nodes.upsert("abc")
            _    <- node.addLabel(Traversal.ontology)
            modifiedOn = Instant.now()
            _ <- node.addOut(Label.P.typed.modifiedonDateTime, modifiedOn)
            _ <- node.addOut(Label.P.`@ids`, "def")
            _ <- node.addOut(Label.P.`@ids`, "gef")
            _ <- for {
              a1 <- g.N
                .has(Label.P.`@modifiedon`, P.gt(Instant.ofEpochSecond(modifiedOn.getEpochSecond - 100)))
                .count
                .withGraph(graph)
                .headF
                .map(_ shouldBe 1)
              a2 <- g.N
                .has(Label.P.`@modifiedon`, P.gt(Instant.ofEpochSecond(modifiedOn.getEpochSecond - 1000)))
                .count
                .withGraph(graph)
                .headF
                .map(_ shouldBe 1)
              a3 <- g.N
                .has(Label.P.`@modifiedon`, P.gt(Instant.ofEpochSecond(modifiedOn.getEpochSecond + 100)))
                .count
                .withGraph(graph)
                .headF
                .map(_ shouldBe 0)
              a4 <- g.N
                .has(
                  Label.P.`@modifiedon`,
                  P.gt(Instant.ofEpochSecond(modifiedOn.getEpochSecond - 1000)) &&
                    P.lt(Instant.ofEpochSecond(modifiedOn.getEpochSecond + 1000))
                )
                .count
                .withGraph(graph)
                .headF
                .map(_ shouldBe 1)
              a5 <- g.N
                .has(Label.P.`@modifiedon`,
                     P.between(Instant.ofEpochSecond(modifiedOn.getEpochSecond - 1000),
                               Instant.ofEpochSecond(modifiedOn.getEpochSecond + 1000)))
                .count
                .withGraph(graph)
                .headF
                .map(_ shouldBe 1)
            } yield ()
          } yield succeed).timeout(1500.millis).runToFuture
//          val traversal  = graph.g.N().hasIri("abc").where(_.hasIri("abc")).limit(10).outMap()
//          val collection = Collection(Instant.now(), Instant.now(), traversal.toList, traversal.ct)
//          collection.item.head.nonEmpty shouldBe true
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
            .timeout(1400.millis)
            .runToFuture
        }
        "contains the person-ontology in cache" in {
          sampleGraph.ns.ontologies.cached(SampleGraph.Person.iri) shouldBe Some(SampleGraph.Person.ontology)
        }
        "contains the person-ontology in the global cache" in {
          Ontology.ontologies.get(SampleGraph.Person.iri) shouldBe Some(SampleGraph.Person.ontology)
        }
        "contains the place-ontology" in {
          sampleGraph.ns.ontologies
            .get(SampleGraph.Place.iri)
            .map { ontologyOption =>
              ontologyOption shouldBe Some(SampleGraph.Place.ontology)
            }
            .timeout(1400.millis)
            .runToFuture
        }
        "contains the place-ontology in cache" in {
          sampleGraph.ns.ontologies.cached(SampleGraph.Place.iri) shouldBe Some(SampleGraph.Place.ontology)
        }
        "contains the place-ontology in the global cache" in {
          Ontology.ontologies.get(SampleGraph.Place.iri) shouldBe Some(SampleGraph.Place.ontology)
        }
        "contains the name-property" in {
          sampleGraph.ns.properties
            .get(SampleGraph.properties.name.iri)
            .map { propertyOption =>
              propertyOption shouldBe Some(SampleGraph.properties.name.property)
            }
            .timeout(1400.millis)
            .runToFuture
        }
        "contains the name-property in cache" in {
          sampleGraph.ns.properties.cached(SampleGraph.properties.name.iri) shouldBe Some(
            SampleGraph.properties.name.property)
        }
        "contains the name-property in the global cache" in {
          Property.properties.get(SampleGraph.properties.name.iri) shouldBe Some(SampleGraph.properties.name.property)
        }
      }
      "have sample data" which {
        "contains certain nodes" in {
          (for {
            garrison <- sampleGraph.nodes.hasIri(sampleGraph.iri + "/person/56789").headL
            a = garrison.labels shouldBe List(person)
            b <- sampleGraph.nodes.count().map(_ shouldBe 10)
          } yield succeed).timeout(4000.millis).runToFuture
        }
        "contains certain edges" in {
          (for {
            yoshio <- sampleGraph.nodes.hasIri(sampleGraph.iri + "/person/123").headL
            _      <- sampleGraph.edges().find(e => e.key == name.property).headL.map(_.key shouldBe name.property)
            _      <- sampleGraph.nodes.count().map(_ shouldBe 10)
            _      <- sampleGraph.edges.count().map(_ shouldBe 58)
          } yield succeed).timeout(4000.millis).runToFuture
        }
        "contains certain values" in {
          sampleGraph.values.count().map(_ shouldBe 38).timeout(1400.millis).runToFuture
        }
      }
      //        "support inserting structures from other graphs (object + edges)" ignore {
      //          val traversal    = graph.ns.g.N().out(Label.P.`@id`).out(Label.P.`@language`)
      //          val node         = traversal.toNode
      //          val upsertedNode = graph.nodes.post(node)
      //          //      graph.ldParser.toJSON.nodeToJsonWithContext(node)._1.toString shouldBe graph.ldParser.toJSON.nodeToJsonWithContext(upsertedNode.asInstanceOf[Node])._1.toString
      //          //      node.property(graph.idUrlString, "abc")
      //          graph.nodes.upsert(node.iri)
      //        }
      "be able to merge" in {
        val newGraph = MemGraph("graphspec2merge")

        (for {
          _           <- newGraph.nodes().toListL.map(_.size shouldBe 0)
          _           <- newGraph.edges().toListL.map(_.size shouldBe 0)
          _           <- newGraph.values().toListL.map(_.size shouldBe 0)
          mergedGraph <- newGraph ++ sampleGraph
          _           <- Task.parZip2(newGraph.nodes.count, sampleGraph.nodes.count).map { case (a, b) => a shouldBe b }
          _           <- Task.parZip2(newGraph.edges.count, sampleGraph.edges.count).map { case (a, b) => a shouldBe b }
          _           <- Task.parZip2(newGraph.values.count, sampleGraph.values.count).map { case (a, b) => a shouldBe b }
        } yield {
          newGraph.close()
          succeed
        }).timeout(4000.millis).runToFuture
      }
    }
  }
}
