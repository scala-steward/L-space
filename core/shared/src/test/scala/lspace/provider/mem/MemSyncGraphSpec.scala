package lspace.provider.mem

import lspace.librarian.task.SyncGuideSpec
import lspace.structure.{Graph, SampledGraph}
import org.scalatest.FutureOutcome

class MemSyncGraphSpec extends SyncGuideSpec {
  implicit lazy val guide = lspace.Implicits.SyncGuide.guide
  import lspace.Implicits.Scheduler.global

  val graph: Graph                    = MemGraph("memgraphspec")
  val sampleGraph                     = SampledGraph(MemGraph("memgraphspec-sample"))
  def createGraph(iri: String): Graph = MemGraph("memgraphspec-" + iri)

  val initTask = (for {
    _ <- sampleGraph.load
  } yield ()).memoizeOnSuccess

  override def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    new FutureOutcome(initTask.runToFuture flatMap { result =>
      super.withFixture(test).toFuture
    })
  }

  sampledGraphComputerTests(sampleGraph)
}
