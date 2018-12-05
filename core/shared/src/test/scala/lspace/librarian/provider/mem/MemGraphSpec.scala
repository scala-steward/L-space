package lspace.librarian.provider.mem

import lspace.librarian.process.computer.GraphComputerSpec
import lspace.librarian.structure._
import lspace.librarian.structure.Property.default._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class MemGraphSpec extends GraphSpec with NodeSpec with GraphComputerSpec {

  //  Ontology
  //  val graph: Graph = MemGraphDefault
  val graph: Graph                    = MemGraph("memgraphspec")
  val sampleGraph: Graph              = MemGraph("memgraphspec-sample")
  def createGraph(iri: String): Graph = MemGraph("memgraphspec-" + iri)

  "MemGraph.default" should {
    "contain all base ontologies and properties" in {
      MemGraphDefault.ns.ontologies.byIri.size should be > 0
      MemGraphDefault.ns.properties.byIri.size should be > 0
    }
    "get 10,000 times from index" in {
      val start = java.time.Instant.now().toEpochMilli
      val id    = sampleGraph.nodes.hasIri("place-san_jose_de_maipo").head.id
      (1 to 10000).foreach(_ => graph.nodes.hasId(id))
      val end      = java.time.Instant.now().toEpochMilli
      val duration = end - start
      println(s"get 10,000 times from index took ${duration} milli-seconds")
    }
    "create 10,000 nodes with an iri" in {
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
      println(s"create 10,000 nodes took ${duration} milli-seconds")
    }
    "create 20,000 nodes with an iri" in {
      val start       = java.time.Instant.now().toEpochMilli
      val transaction = graph.transaction
      (1 to 20000).foreach { i =>
        val node = transaction.nodes.create()
        node --- `@id` --> s"some-iri-20,000-$i"
      }
      transaction
        .commit()

      val end      = java.time.Instant.now().toEpochMilli
      val duration = end - start
      println(s"create 20,000 nodes took ${duration} milli-seconds")
    }
//    "create 2x parallel 10,000 nodes with an iri" in {
//      val start = java.time.Instant.now().toEpochMilli
//
//      Await.result(
//        Future.sequence(
//          1.to(5)
//            .flatMap(i =>
//              Seq(
//                Future {
//                  val transaction1 = graph.transaction
//                  (1 to 10000).foreach { i =>
//                    val node = transaction1.nodes.upsert(s"some-iri-10,000-1-$i")
////                    node --- `@id` --> s"some-iri-10,000-1-$i"
//                  }
//                  transaction1
//                    .commit()
//                },
//                Future {
//                  val transaction2 = graph.transaction
//                  (1 to 10000).foreach { i =>
//                    val node = transaction2.nodes.create()
//                    node --- `@id` --> s"some-iri-10,000-2-$i"
//                  }
//                  transaction2
//                    .commit()
//                }
//            ))),
//        300 seconds
//      )
//      val end      = java.time.Instant.now().toEpochMilli
//      val duration = end - start
//      println(s"nodes in graph #${graph.nodes.count()}")
//      println(s"create 2x 10,000 nodes in parallel took ${duration} milli-seconds")
//    }
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
