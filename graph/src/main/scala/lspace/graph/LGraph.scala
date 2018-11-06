package lspace.graph

import monix.eval.Task
import lspace.librarian.process.traversal.Traversal
import lspace.librarian.structure._
import shapeless.HList

object LGraph {
  def apply(_iri: String): LGraph = {
    val graph = new LGraphDataGraph {
      val iri: String  = _iri
      private val self = this
      val ns: LGraphNSGraph = new LGraphNSGraph {
        def iri: String   = _iri + ".ns"
        val graph: LGraph = self
      }
      init()
    }

    graph
  }
}

trait LGraphDataGraph extends LGraph with DataGraph {

  def init(): Unit = {}
}

trait LGraphNSGraph extends LGraph with NameSpaceGraph {
  def graph: LGraph

  def init(): Unit = {}
}

trait LGraph extends Graph {

  /**
    * Links A.K.A. Edges A.K.A. Properties
    *
    * @return
    */
  override def links: Stream[Edge[_, _]] = ???

  /**
    * Nodes A.K.A. Vertices
    *
    * @return
    */
  override def nodes: Stream[Node]                                                                        = ???
  override def values: Stream[Value[_]]                                                                   = ???
  override protected def _createNode(ontology: Ontology*): Node                                           = ???
  override protected def _createEdge[S, E](from: Resource[S], key: Property, to: Resource[E]): Edge[S, E] = ???
  override protected def _createValue[T](value: T)(dt: DataType[T]): Value[T]                             = ???

  override protected def _deleteNode(node: Node): Unit       = ???
  override protected def _deleteEdge(edge: Edge[_, _]): Unit = ???
  override protected def _deleteValue(value: Value[_]): Unit = ???

  override def getNodeById(id: Long): Option[Node]                = ???
  override def getEdgeById(id: Long): Option[Edge[_, _]]          = ???
  override def getValueById(id: Long): Option[Value[_]]           = ???
  override def getResourceById(id: Long): Option[Resource[_]]     = ???
  override def upsertResource[V](value: Resource[V]): Resource[V] = ???
  override def buildTraversersStream[ST <: ClassType[_], ET <: ClassType[_], Steps <: HList, Out](
      traversal: Traversal[ST, ET, Steps])(ct: ClassType[_]): Stream[Out] = ???
  override def buildAsyncTraversersStream[ST <: ClassType[_], ET <: ClassType[_], Steps <: HList, Out](
      traversal: Traversal[ST, ET, Steps])(ct: ClassType[_]): Task[Stream[Out]] = ???
}
