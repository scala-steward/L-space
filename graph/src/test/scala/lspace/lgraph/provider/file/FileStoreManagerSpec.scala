package lspace.lgraph.provider.file

import lspace.lgraph.LGraph
import lspace.lgraph.provider.mem.MemIndexProvider
import lspace.librarian.task.{AsyncGuide, AsyncGuideSpec, Guide}
import lspace.structure._
import monix.eval.Task
import org.scalatest.FutureOutcome
import scribe.Level

class FileStoreManagerSpec extends GraphSpec with NodeSpec with AsyncGuideSpec with NameSpaceGraphSpec {
  implicit val guide = lspace.Implicits.AsyncGuide.guide
  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

  implicit val baseEncoder = lspace.codec.argonaut.nativeEncoder
  implicit val baseDecoder = lspace.codec.argonaut.nativeDecoder
  implicit val httpClient  = lspace.client.io.HttpClientAsyncHttp

  scribe.Logger.root.clearHandlers().clearModifiers().withHandler(minimumLevel = Some(Level.Warn)).replace()

  def createGraph(iri: String): Graph = {
    val storage = FileStoreProvider(iri, "_data/" + iri)
    LGraph(storage, new MemIndexProvider, noinit = true)
  }

  lazy val initTask = (for {
    _ <- Task {
      val directory = new java.io.File("_data")
      def deleteAll(file: java.io.File): Unit =
        try if (file.exists()) {
          if (file.isDirectory && file.listFiles().toList.nonEmpty)
            file
              .listFiles()
              .toList
              .filter(_.exists())
              .filter(_ != null)
              .foreach(f => deleteAll(f))
          file.delete()
        } catch {
          case e: Throwable => scribe.warn(e.getMessage)
        }
      deleteAll(directory)
    }
    _ <- sampleGraph.load
    _ <- graphToPersist.load
    _ <- graphToPersist.graph.persist
    _ <- graphToPersist.graph.close()
//    _ = {
//      lspace.structure.Ontology.ontologies.byIri.clear()
//      lspace.structure.Property.properties.byIri.clear()
//      lspace.datatype.DataType.datatypes.byIri.clear()
//      lspace.datatype.DataType.datatypes.building.clear()
//    }
    _ <- samplePersistedGraph.graph.init
  } yield ()).memoize

  lazy val graph: Graph         = createGraph("FileStoreManagerSpec")
  lazy val sampleGraph          = SampledGraph(createGraph("FileStoreManagerSpec-sample"))
  lazy val graphToPersist       = SampledGraph(createGraph("FileStoreManagerSpec-persisted-sample"))
  lazy val samplePersistedGraph = SampledGraph(createGraph("FileStoreManagerSpec-persisted-sample"))

  override def withFixture(test: NoArgAsyncTest): FutureOutcome =
    new FutureOutcome(initTask.runToFuture.flatMap { result =>
      super.withFixture(test).toFuture
    })

  "FileStoreManager" when {
    "new" should {
      nameSpaceGraphTests(graph)
      graphTests(graph)
      sampledGraphTests(sampleGraph)
      nodeTests(graph)
      sampledNodeTests(sampleGraph)
      sampledGraphComputerTests(sampleGraph)
    }
    "persisted" should {
      sampledGraphTests(samplePersistedGraph)
      sampledNodeTests(samplePersistedGraph)
      sampledGraphComputerTests(samplePersistedGraph)
    }
  }
}
