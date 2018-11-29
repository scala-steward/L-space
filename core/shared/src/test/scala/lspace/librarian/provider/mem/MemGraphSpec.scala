package lspace.librarian.provider.mem

import lspace.librarian.structure._

class MemGraphSpec extends GraphSpec {
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
      (1 to 10000).foreach(_ => graph.nodes.hasIri("sptth/tbd.tld/librarian/step/HasLabel"))
      val end      = java.time.Instant.now().toEpochMilli
      val duration = end - start
      println(s"get 10,000 times from index took ${duration} milli-seconds")
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
