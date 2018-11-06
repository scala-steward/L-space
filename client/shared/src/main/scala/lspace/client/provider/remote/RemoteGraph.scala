package lspace.client.provider.remote

import monix.eval.Task
import lspace.client.io.LinkedDataService
import lspace.librarian.process.traversal.Traversal
import lspace.librarian.provider.mem.{MemGraphDefault, MemNSGraph}
import lspace.librarian.structure._
import shapeless.HList

object RemoteGraph {
  def apply(_iri: String, cache0: Graph)(implicit _service: LinkedDataService): RemoteGraph = new RemoteGraph {
    val iri: String = _iri
    val cache       = cache0
    val service     = _service
  }
}

trait RemoteGraph extends Graph {

  def cache: Graph
  lazy val ns: NameSpaceGraph = cache.ns

  def service: LinkedDataService

  def links: Stream[Edge[_, _]] = throwDoesNotSupportSyncTraversal //g.E.toStream
  def nodes: Stream[Node]       = throwDoesNotSupportSyncTraversal //g.V.toStream
  def values: Stream[Value[_]]  = throwDoesNotSupportSyncTraversal //g.VR.toStream

  protected def _createNode(ontology: Ontology*): Node =
    throw new Exception("remote graphs do not (yet) support writing")
  protected def _createEdge[S, E](from: Resource[S], key: Property, to: Resource[E]): Edge[S, E] =
    throw new Exception("remote graphs do not (yet) support writing")
  def newValue[T](dataType: LiteralType[T], value: T): Value[T] =
    throw new Exception("remote graphs do not (yet) support writing")
  override def _createValue[T](value: T)(dt: DataType[T]): Value[T] =
    throw new Exception("remote graphs do not (yet) support writing")

  protected def _deleteNode(node: Node): Unit       = ???
  protected def _deleteEdge(edge: Edge[_, _]): Unit = ???
  protected def _deleteValue(value: Value[_]): Unit = ???

  override def upsertNode(uri: String, uris: Set[String]): Node =
    throw new Exception("remote graphs do not (yet) support writing")
//  override def postResource[V](resource: Resource[V]): Resource[V] =
//    throw new Exception("remote graphs do not (yet) support writing")

  override def init(): Unit = {} //TODO: test connection?

  override def getNodeById(id: Long): Option[Node]                = ???
  override def getEdgeById(id: Long): Option[Edge[_, _]]          = ???
  override def getValueById(id: Long): Option[Value[_]]           = ???
  override def getResourceById(id: Long): Option[Resource[_]]     = ???
  override def upsertResource[V](value: Resource[V]): Resource[V] = ???

  private def throwDoesNotSupportSyncTraversal =
    throw new Exception("remote graph traversals do not support synchronous calls")

  def buildTraversersStream[ST <: ClassType[_], ET <: ClassType[_], Steps <: HList, Out](
      traversal: Traversal[ST, ET, Steps])(ct: ClassType[_]): Stream[Out] =
    ???

  def buildAsyncTraversersStream[ST <: ClassType[_], ET <: ClassType[_], Steps <: HList, Out](
      traversal: Traversal[ST, ET, Steps])(ct: ClassType[_]): Task[Stream[Out]] =
    service.traverse[Steps, Out](traversal, ct.asInstanceOf[ClassType[Out]]).map(_.item.toStream)
}
