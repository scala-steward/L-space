package lspace.lgraph.provider.mem

import lspace.lgraph.LGraph
import lspace.librarian.task.{AsyncGuide, AsyncGuideSpec, Guide}
import lspace.structure.{Graph, GraphSpec, NodeSpec, SampledGraph}
import org.scalatest.FutureOutcome

class MemStoreManagerSpec extends GraphSpec with NodeSpec with AsyncGuideSpec {
  implicit lazy val guide = lspace.Implicits.AsyncGuide.guide
  import lspace._
  import lspace.Implicits.Scheduler.global

  def createGraph(iri: String): Graph = {
    val storage = MemStoreProvider(iri)
    LGraph(storage, new MemIndexProvider)
  }

  lazy val graph: Graph = createGraph("MemStoreManagerSpec")
  lazy val sampleGraph  = SampledGraph(createGraph("MemStoreManagerSpec-sample"))

  val initTask = (for {
    u <- sampleGraph.load
  } yield ()).memoizeOnSuccess

  override def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    new FutureOutcome(initTask.runToFuture flatMap { result =>
      super.withFixture(test).toFuture
    })
  }

  graphTests(graph)
  sampledGraphTests(sampleGraph)
  nodeTests(graph)
  sampledNodeTests(sampleGraph)
  sampledGraphComputerTests(sampleGraph)
}
