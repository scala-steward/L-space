package lspace.client.provider.remote

import monix.eval.Task
import lspace.client.io.LinkedDataService
import lspace.librarian.datatype.DataType
import lspace.librarian.process.traversal.Traversal
import lspace.librarian.provider.transaction.Transaction
import lspace.librarian.structure._
import lspace.librarian.structure.index.Index
import lspace.librarian.structure.store.{EdgeStore, NodeStore, Store, ValueStore}
import lspace.librarian.structure.util.IdProvider
import shapeless.HList

object RemoteGraph {
  def apply(_iri: String, cache0: Graph)(implicit _service: LinkedDataService): RemoteGraph = new RemoteGraph {
    val iri: String = _iri
    val cache       = cache0
    val service     = _service
  }
}

trait RemoteGraph extends Graph {

  lazy val idProvider: IdProvider = ???

  def cache: Graph
  lazy val ns: NameSpaceGraph = cache.ns

  def service: LinkedDataService

  protected def nodeStore: NodeStore[this.type]   = ???
  protected def edgeStore: EdgeStore[this.type]   = ???
  protected def valueStore: ValueStore[this.type] = ???
  override def edges: Edges                       = throw new Exception("remote graph has no local edges") //g.E.toStream
  override def nodes: Nodes                       = throw new Exception("remote graph has no local nodes") //g.V.toStream
  override def values: Values                     = throw new Exception("remote graph has no local values:") //g.VR.toStream

  override protected def newNode(id: Long): GNode = throw new Exception("remote graphs do not (yet) support writing")
  override protected def getOrCreateNode(id: Long): GNode =
    throw new Exception("remote graphs do not (yet) support writing")
  override protected def newEdge(id: Long, from: Long, key: Property, to: Long): GEdge[Any, Any] =
    throw new Exception("remote graphs do not (yet) support writing")
  override protected def newEdge[S, E](id: Long, from: GResource[S], key: Property, to: GResource[E]): GEdge[S, E] =
    throw new Exception("remote graphs do not (yet) support writing")
  protected def _createEdge[S, E](id: Long)(from: Resource[S], key: Property, to: Resource[E]): Edge[S, E] =
    throw new Exception("remote graphs do not (yet) support writing")

  override protected def newValue[T](id: Long, value: T, label: DataType[T]): GValue[T] =
    throw new Exception("remote graphs do not (yet) support writing")
//  def newValue[T](dataType: LiteralType[T], value: T): Value[T] =
//    throw new Exception("remote graphs do not (yet) support writing")
  override def createValue[T](id: Long, value: T, dt: DataType[T]): GValue[T] =
    throw new Exception("remote graphs do not (yet) support writing")

  /**
    * creates new transaction
    *
    * @return
    */
  override def transaction: Transaction = ???

//  protected def `@idStore`: ValueStore[this.type] = ???

  override protected def createEdge[S, E](id: Long, from: GResource[S], key: Property, to: GResource[E]): GEdge[S, E] =
    ???

  override protected def deleteResource[T <: _Resource[_]](resource: T): Unit = ???

  protected def _deleteNode(node: Node): Unit       = ???
  protected def _deleteEdge(edge: Edge[_, _]): Unit = ???
  protected def _deleteValue(value: Value[_]): Unit = ???

//  override def postResource[V](resource: Resource[V]): Resource[V] =
//    throw new Exception("remote graphs do not (yet) support writing")

  override def init(): Unit = {} //TODO: test connection?

  private def throwDoesNotSupportSyncTraversal =
    throw new Exception("remote graph traversals do not support synchronous calls")

  def buildTraversersStream[ST <: ClassType[_], ET <: ClassType[_], Steps <: HList, Out](
      traversal: Traversal[ST, ET, Steps]): Stream[Out] =
    ???

  def buildAsyncTraversersStream[ST <: ClassType[_], ET <: ClassType[_], Steps <: HList, Out](
      traversal: Traversal[ST, ET, Steps]): Task[Stream[Out]] =
    service.traverse[Steps, Out](traversal).map(_.item.toStream)
}
