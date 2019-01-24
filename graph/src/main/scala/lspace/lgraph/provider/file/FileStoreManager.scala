package lspace.lgraph.provider.file

import lspace.lgraph._
import lspace.lgraph.store.StoreManager
import lspace.librarian.datatype._
import lspace.librarian.structure.Property
import monix.eval.Task
import monix.tail.Iterant

import scala.io.BufferedSource

object FileStoreManager {
  object graphnames {
    val graph = "graph"
    object data {
      val nodes  = "nodes"
      val edges  = "edges"
      val values = "values"
    }
    object ns {
      val nodes  = "nodes"
      val edges  = "edges"
      val values = "values"
    }
  }
  def apply[G <: LGraph](graph: G, path: String): FileStoreManager[G] = new FileStoreManager[G](graph, path) {}
}

/**
  * This manager stores all resources to a filesystem. It builds a the complete graph in memory on initialization
  * and persists (async) on any commits to the graph.
  * @param graph
  * @tparam G
  */
class FileStoreManager[G <: LGraph](override val graph: G, path: String) extends StoreManager(graph) {
  private val directory = new java.io.File(path)
  directory.mkdirs
  if (!directory.isDirectory) throw new Exception(s"storage path ${directory.getAbsolutePath} is not a directory")

  private def openFile(filename: String): Iterant[Task, BufferedSource] = {
    Iterant[Task].resource {
      Task(scala.io.Source.fromFile(s"$path/$filename"))
    } { in =>
      Task(in.close())
    }
  }
  private object graphfiles {
    import FileStoreManager.graphnames
    def graph = openFile(graphnames.graph)
    object data {
      def nodes  = openFile(graphnames.data.nodes)
      def edges  = openFile(graphnames.data.edges)
      def values = openFile(graphnames.data.values)
    }
    object ns {
      def nodes  = openFile(graphnames.ns.nodes)
      def edges  = openFile(graphnames.ns.edges)
      def values = openFile(graphnames.ns.values)
    }
  }

  override def nodeById(id: Long): Option[graph._Node with LNode] = None

  override def nodesById(ids: List[Long]): Stream[graph._Node with LNode] = Stream()

  override def nodeByIri(iri: String): Stream[graph._Node with LNode] = Stream()

  override def nodesByIri(iri: List[String]): Stream[graph._Node with LNode] = Stream()

  override def edgeById(id: Long): Option[graph._Edge[Any, Any] with LEdge[Any, Any]] = None

  override def edgesById(ids: List[Long]): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def edgesByFromId(fromId: Long): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def edgesByFromIdAndKey(fromId: Long, key: Property): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] =
    Stream()

  override def edgesByToId(toId: Long): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def edgesByToIdAndKey(toId: Long, key: Property): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] =
    Stream()

  override def edgesByFromIdAndToId(fromId: Long, toId: Long): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] =
    Stream()

  override def edgesByFromIdAndKeyAndToId(fromId: Long,
                                          key: Property,
                                          toId: Long): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def edgeByIri(iri: String): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def edgesByIri(iri: List[String]): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def valueById(id: Long): Option[graph._Value[Any] with LValue[Any]] = None

  override def valuesById(ids: List[Long]): Stream[graph._Value[Any] with LValue[Any]] = Stream()

  override def valueByIri(iri: String): Stream[graph._Value[Any] with LValue[Any]] = Stream()

  override def valuesByIri(iri: List[String]): Stream[graph._Value[Any] with LValue[Any]] = Stream()

  override def valueByValue[T](value: T, dt: DataType[T]): Stream[graph._Value[T] with LValue[T]] = Stream()

  override def valuesByValue[T](values: List[(T, DataType[T])]): Stream[graph._Value[T] with LValue[T]] = Stream()

  override def storeNodes(nodes: List[graph._Node with LNode]): Task[_] = Task {}

  override def storeEdges(
      edges: List[(graph._Edge[_$1, _$2] with LEdge[_$1, _$2]) forSome { type _$1; type _$2 }]): Task[_] =
    Task {}

  override def storeValues(values: List[(graph._Value[_$1] with LValue[_$1]) forSome { type _$1 }]): Task[_] =
    Task {}

  override def deleteNodes(nodes: List[graph._Node with LNode]): Task[_] =
    Task {}

  override def deleteEdges(
      edges: List[(graph._Edge[_$1, _$2] with LEdge[_$1, _$2]) forSome { type _$1; type _$2 }]): Task[_] =
    Task {}

  override def deleteValues(values: List[(graph._Value[_$1] with LValue[_$1]) forSome { type _$1 }]): Task[_] =
    Task {}

  override def nodes: Stream[graph._Node with LNode] = Stream()

  override def edges: Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def values: Stream[graph._Value[Any] with LValue[Any]] = Stream()

  override def nodeCount(): Long = graph.nodes().size

  override def edgeCount(): Long = graph.edges().size

  override def valueCount(): Long = graph.values().size

  def init(): Unit = {}

  /**
    * finishes write-queue(s) and closes connection
    */
  override def close(): Unit = {}
}
