package lspace.lgraph.provider.cassandra

import lspace.lgraph.LGraph
import lspace.lgraph.provider.mem.MemIndexProvider
import lspace.librarian.task.AsyncGuideSpec
import lspace.structure.{Graph, GraphSpec, NameSpaceGraphSpec, NodeSpec, SampledGraph}
import monix.eval.Task
import org.scalatest.FutureOutcome

class CassandraStoreManagerSpec extends GraphSpec with NodeSpec with AsyncGuideSpec with NameSpaceGraphSpec {

  implicit val guide = lspace.Implicits.AsyncGuide.guide
  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

  implicit val baseEncoder = lspace.codec.argonaut.nativeEncoder
  implicit val baseDecoder = lspace.codec.argonaut.nativeDecoder

  def createGraph(iri: String): Graph = {
    val storage = LCassandraStoreProvider(iri, "localhost", 9042)
    LGraph(storage, new MemIndexProvider, noinit = true)
  }

  val store       = LCassandraStoreProvider("CassandraStorageManagerSpec", "localhost", 9042)
  val sampleStore = LCassandraStoreProvider("CassandraStorageManagerSpec-sample", "localhost", 9042)

  lazy val initTask = (for {
    _ <- Task.parSequenceUnordered(
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
          _ = Thread.sleep(10000)
          _ <- samplePersistedGraph.graph.init
        } yield ()
      ))
  } yield ()).memoize //OnSuccess

  lazy val graph: LGraph =
    LGraph(store, new MemIndexProvider, noinit = true)
  lazy val sampleGraph          = SampledGraph(createGraph("CassandraStorageManagerSpec-sample"))
  lazy val graphToPersist       = SampledGraph(createGraph("CassandraStorageManagerSpec-persisted-sample"))
  lazy val samplePersistedGraph = SampledGraph(createGraph("CassandraStorageManagerSpec-persisted-sample"))

  override def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    new FutureOutcome(initTask.runToFuture flatMap { result =>
      super.withFixture(test).toFuture
    })
  }

  "CassandraStoreManagerSpec" when {
    "new" should {
//      nameSpaceGraphTests(graph)
//      graphTests(graph)
//      nodeTests(graph)
//      sampledGraphTests(sampleGraph)
//      sampledNodeTests(sampleGraph)
//      sampledGraphComputerTests(sampleGraph)
    }
    "persisted" should {
      sampledGraphTests(samplePersistedGraph)
      sampledNodeTests(samplePersistedGraph)
      sampledGraphComputerTests(samplePersistedGraph)
    }
  }
}
