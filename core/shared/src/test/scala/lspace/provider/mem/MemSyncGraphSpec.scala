package lspace.provider.mem

import lspace.librarian.task.SyncGuideSpec
import lspace.structure.{Graph, SampledGraph}
import org.scalatest.FutureOutcome

class MemSyncGraphSpec extends SyncGuideSpec {
  implicit lazy val guide = lspace.Implicits.SyncGuide.guide
  import lspace.Implicits.Scheduler.global

  val graph: Graph                    = MemGraph("MemSyncGraphSpec")
  val sampleGraph                     = SampledGraph(MemGraph("MemSyncGraphSpec-sample"))
  def createGraph(iri: String): Graph = MemGraph("MemSyncGraphSpec-" + iri)

  lazy val initTask = (for {
    _ <- sampleGraph.load
  } yield ()).memoizeOnSuccess

  override def withFixture(test: NoArgAsyncTest): FutureOutcome =
    new FutureOutcome(initTask.runToFuture.flatMap { _ =>
      super.withFixture(test).toFuture
    })

//  "ab" should {
//    "de" in Future { succeed }
//  }
  sampledGraphComputerTests(sampleGraph)
}
