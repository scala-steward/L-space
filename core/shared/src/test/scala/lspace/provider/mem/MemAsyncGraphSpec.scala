package lspace.provider.mem

import lspace.librarian.task.AsyncGuideSpec
import lspace.structure._
import org.scalatest.FutureOutcome

class MemAsyncGraphSpec extends GraphSpec with NodeSpec with AsyncGuideSpec with NameSpaceGraphSpec {
  import lspace.Implicits.Scheduler.global

  implicit lazy val guide = lspace.Implicits.AsyncGuide.guide
  import lspace._

  val graph: Graph                    = MemGraph("MemAsyncGraphSpec")
  val sampleGraph                     = SampledGraph(MemGraph("MemAsyncGraphSpec-sample"))
  def createGraph(iri: String): Graph = MemGraph("MemAsyncGraphSpec-" + iri)

  val initTask = (for {
    sample <- sampleGraph.load
  } yield sample).memoizeOnSuccess

  override def withFixture(test: NoArgAsyncTest): FutureOutcome =
    new FutureOutcome(initTask.runToFuture.flatMap { _ =>
      super.withFixture(test).toFuture
    })

  nameSpaceGraphTests(graph)
  graphTests(graph)
  sampledGraphTests(sampleGraph)
  nodeTests(graph)
  sampledNodeTests(sampleGraph)
  sampledGraphComputerTests(sampleGraph)
}
