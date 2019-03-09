package lspace.lgraph.provider.file

import lspace.datatype.{DataType, ListType}
import lspace.lgraph.LGraph
import lspace.lgraph.provider.mem.MemIndexProvider
import lspace.librarian.task.{AsyncGuide, AsyncGuideSpec, Guide}
import lspace.structure._
import lspace.util.SampleGraph
import monix.eval.Task

import scala.concurrent.Await

class FileStoreManagerSpec extends GraphSpec with NodeSpec with AsyncGuideSpec with NameSpaceGraphSpec {
  implicit val guide = lspace.Implicits.AsyncGuide.guide

  implicit val baseEncoder = lspace.codec.argonaut.nativeEncoder
  implicit val baseDecoder = lspace.codec.argonaut.nativeDecoder

  def createGraph(iri: String): Graph = {
    val storage = FileStoreProvider(iri, "_data/" + iri)
    LGraph(storage, new MemIndexProvider)
  }

  val directory = new java.io.File("_data")
  def deleteAll(file: java.io.File): Unit = {
    try {
      if (file.exists()) {
        if (file.isDirectory && file.listFiles().toList.nonEmpty)
          file
            .listFiles()
            .toList
            .filter(_.exists())
            .filter(_ != null)
            .foreach(f => deleteAll(f))
        file.delete()
      }
    } catch {
      case e => scribe.warn(e.getMessage)
    }
  }
  deleteAll(directory)

  lazy val graph: Graph = createGraph("FileStoreManagerSpec")
  lazy val sampleGraph  = SampledGraph(createGraph("FileStoreManagerSpec-sample"))
  sampleGraph.load
  import scala.concurrent.duration._
  lazy val graphToPersist = SampledGraph(createGraph("FileStoreManagerSpec-persisted-sample"))
  graphToPersist.load
  Await.ready(graphToPersist.graph.persist, 10 seconds)
  Await.ready(graphToPersist.graph.close(), 10 seconds)

  lspace.structure.Ontology.ontologies.byIri.clear()
  lspace.structure.Property.properties.byIri.clear()
  lspace.datatype.DataType.datatypes.byIri.clear()
  lspace.datatype.DataType.datatypes.building.clear()
  lazy val samplePersistedGraph = SampledGraph(createGraph("FileStoreManagerSpec-persisted-sample"))
  Await.ready(samplePersistedGraph.graph.init, 20 seconds)

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
