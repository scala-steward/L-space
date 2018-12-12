package lspace.librarian.structure

import lspace.librarian.process.traversal.P
import lspace.librarian.structure.Property.default._
import lspace.librarian.util.SampleGraph
import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, Filter, Matchers}

import scala.concurrent.Future

trait AsyncGraphSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {
  implicit override def executionContext = scala.concurrent.ExecutionContext.Implicits.global

  def graph: Graph
  def sampleGraph: Graph
  def createGraph(iri: String): Graph
  def createSampleGraph(iri: String): Graph = {
    val graph = createGraph(iri)
    SampleGraph.loadSocial(graph)
    graph
  }

  override val invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected = true

  override def beforeAll = {
    if (expectedTestCount(Filter()) > 0) {
      SampleGraph.loadSocial(sampleGraph)
    }
  }

  override def afterAll(): Unit = {
    graph.close()
    sampleGraph.close()
  }
  private[this] val newIdsLock = new Object
  import scala.collection.JavaConverters._
  "AGraph" can {
    "create nodes in parallel" in {
      val newIds: scala.collection.concurrent.Map[Long, List[Long]] =
        new java.util.concurrent.ConcurrentHashMap[Long, List[Long]]().asScala

      val start = java.time.Instant.now().toEpochMilli
      Future {
        (1 to 1000).map { i =>
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
                      newIdsLock.synchronized {
                        newIds += 0l -> (node.id :: newIds.getOrElse(0l, List()))
                      }

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
  }
}
