package lspace.lgraph.provider.elasticsearch

import lspace.lgraph.LGraph
import lspace.lgraph.index.{LGraphIndexSpec, LIndex}
import lspace.lgraph.provider.mem.MemStoreProvider
import lspace.librarian.traversal.UntypedTraversal
import lspace.structure.SampledGraph
import monix.eval.Task
import org.scalatest.FutureOutcome

class ESIndexSpec extends LGraphIndexSpec {

  implicit val guide = lspace.Implicits.AsyncGuide.guide
  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

  implicit val baseEncoder = lspace.codec.argonaut.nativeEncoder
  implicit val baseDecoder = lspace.codec.argonaut.nativeDecoder

  def createIndex(traversal: UntypedTraversal): Task[LIndex] = Task.now(LIndex(traversal))
  def createGraph(iri: String) = {
    val storage = MemStoreProvider(iri)
    LGraph(storage, ESIndexProvider("http", "localhost", 9200), noinit = true)
  }

  val graph                     = createGraph("ESIndexSpec")
  val sampleGraph               = SampledGraph(createGraph("ESIndexSpec-sample"))
  lazy val graphToPersist       = SampledGraph(createGraph("ESIndexSpec-persisted-sample"))
  lazy val samplePersistedGraph = SampledGraph(createGraph("ESIndexSpec-persisted-sample"))

  val initTask = (for {
    _ <- Task.gatherUnordered(
      Seq(
        for {
          _ <- graph.init
          _ <- graph.purge
        } yield (),
        for {
          _ <- sampleGraph.graph.init
          _ <- sampleGraph.graph.purge
          _ <- sampleGraph.load
        } yield (),
        for {
          _ <- graphToPersist.graph.init
          _ <- graphToPersist.graph.purge
          _ <- graphToPersist.load
          _ <- graphToPersist.graph.persist
          _ <- graphToPersist.graph.close()
          _ <- samplePersistedGraph.graph.init
        } yield ()
      ))
  } yield ()).memoize

  override def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    new FutureOutcome(initTask.runToFuture flatMap { result =>
      super.withFixture(test).toFuture
    })
  }

  "ESIndexManager" when {
    "new" should {
      indexTests(graph)
//      nameSpaceGraphTests(graph)
//      graphTests(graph)
//      sampledGraphTests(sampleGraph)
//      nodeTests(graph)
//      sampledNodeTests(sampleGraph)
//      sampledGraphComputerTests(sampleGraph)
    }
    "persisted" should {
//      sampledGraphTests(samplePersistedGraph)
//      sampledNodeTests(samplePersistedGraph)
//      sampledGraphComputerTests(samplePersistedGraph)
    }
  }
}
