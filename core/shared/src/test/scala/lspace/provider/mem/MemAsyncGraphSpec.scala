package lspace.provider.mem

import lspace.librarian.task.AsyncGuideSpec
import lspace.structure._
import lspace.structure.Property.default._
import org.scalatest.{FutureOutcome, Stopper}

class MemAsyncGraphSpec extends GraphSpec with NodeSpec with AsyncGuideSpec with NameSpaceGraphSpec {
  import lspace.Implicits.Scheduler.global

  implicit lazy val guide = lspace.Implicits.AsyncGuide.guide
  import lspace._

  val graph: Graph                    = MemGraph("memgraphspec")
  val sampleGraph                     = SampledGraph(MemGraph("memgraphspec-sample"))
  def createGraph(iri: String): Graph = MemGraph("memgraphspec-" + iri)

  val initTask = (for {
    sample <- sampleGraph.load
  } yield sample).memoizeOnSuccess

  override def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    new FutureOutcome(initTask.runToFuture flatMap { result =>
      super.withFixture(test).toFuture
    })
  }

  nameSpaceGraphTests(graph)
  graphTests(graph)
  sampledGraphTests(sampleGraph)
  nodeTests(graph)
  sampledNodeTests(sampleGraph)
  sampledGraphComputerTests(sampleGraph)
}
