package lspace.librarian.provider.mem

import lspace.NS
import lspace.librarian.datatype.DataType
import monix.eval.Task
import lspace.librarian.process.computer.DefaultStreamComputer
import lspace.librarian.process.traversal._
import lspace.librarian.provider.mem.store.{MemEdgeStore, MemNodeStore, MemValueStore}
import lspace.librarian.provider.transaction.Transaction
import lspace.librarian.structure._
import lspace.librarian.structure.store.{EdgeStore, NodeStore, ValueStore}
import lspace.librarian.structure.util.IdProvider
import monix.execution.atomic.Atomic
import shapeless.{::, HList}

object MemGraph {

  lazy val default: MemGraph = {
    //    Graph.graphs += "default" -> MemGraphDefault
    MemGraphDefault
  }

  def apply(_iri: String): MemGraph = {
    val graph = new MemDataGraph {
      val iri: String = _iri

      lazy val idProvider: IdProvider = new IdProvider {
        private val id = Atomic(1000l)
        def next: Long = id.incrementAndGet()
      }

      private lazy val self = this
      lazy val ns: MemNSGraph = new MemNSGraph {
        def iri: String = _iri + ".ns"

        lazy val graph: MemGraph    = self
        private lazy val _thisgraph = thisgraph
        lazy val index: MemIndexGraph = new MemIndexGraph {
          def iri: String = _iri + ".ns" + ".index"

          lazy val graph: MemGraph      = _thisgraph
          lazy val index: MemIndexGraph = this
        }
      }
      lazy val index: MemIndexGraph = new MemIndexGraph {
        def iri: String = _iri + ".index"

        lazy val graph: MemGraph    = self
        private lazy val _thisgraph = thisgraph
        lazy val index: MemIndexGraph = new MemIndexGraph {
          def iri: String = _iri + ".index" + ".index"

          lazy val graph: MemGraph      = _thisgraph
          lazy val index: MemIndexGraph = this
        }
      }
      init
    }

    graph
  }

}

trait MemGraph extends Graph {
  type GResource[T] = _Resource[T] with MemResource[T]
  type GNode        = _Node with MemNode //with GResource[Node]
  type GEdge[S, E]  = _Edge[S, E] with MemEdge[S, E] //with GResource[Edge[S, E]]
  type GValue[T]    = _Value[T] with MemValue[T] //with GResource[T]

  def transaction: Transaction = MemTransaction(thisgraph)

  protected[mem] val nodeStore: MemNodeStore[this.type]   = MemNodeStore("@node", thisgraph)
  protected[mem] val edgeStore: MemEdgeStore[this.type]   = MemEdgeStore("@edge", thisgraph)
  protected[mem] val valueStore: MemValueStore[this.type] = MemValueStore("@edge", thisgraph)

  protected[mem] val `@idStore`: ValueStore[this.type] =
    MemValueStore(NS.types.`@id`, thisgraph)

  private[this] val newNodeLock = new Object
  protected[mem] def newNode(id: Long): GNode = newNodeLock.synchronized {
    nodeStore
      .hasId(id)
      .getOrElse {
        def _id = id
        val node = new _Node with MemNode {
          val id              = _id
          val graph: MemGraph = thisgraph
        }
        nodeStore.store(node.asInstanceOf[GNode])
        node
      }
      .asInstanceOf[GNode]
  }

  override protected[mem] def storeNode(node: GNode): Unit = super.storeNode(node)

  private[this] val newEdgeLock = new Object
  protected[mem] def newEdge[S, E](id: Long, from: GResource[S], key: Property, to: GResource[E]): GEdge[S, E] =
    newEdgeLock
      .synchronized {
        edgeStore.hasId(id).getOrElse {
          def _id   = id
          def _from = from
          def _key  = key
          def _to   = to
          val edge = new _Edge[S, E] with MemEdge[S, E] {
            val id: Long           = _id
            val from: GResource[S] = _from
            val key: Property      = _key
            val to: GResource[E]   = _to
            val graph: MemGraph    = thisgraph
          }
          edgeStore.store(edge.asInstanceOf[GEdge[_, _]])
          edge
        }
      }
      .asInstanceOf[GEdge[S, E]]

  protected[mem] def newEdge(id: Long, from: Long, key: Property, to: Long): GEdge[Any, Any] = {
    val _from = resources
      .hasId(from)
      .map(_.asInstanceOf[GResource[Any]])
      .getOrElse {
        throw new Exception(s"cannot create edge, from-resource with id ${from} not found")
      }
    val _to =
      resources
        .hasId(to)
        .map(_.asInstanceOf[GResource[Any]])
        .getOrElse {
          throw new Exception(s"cannot create edge, to-resource with id ${to} not found")
        }

    val edge = createEdge(id, _from, key, _to)
    edge.asInstanceOf[GEdge[Any, Any]]
  }

  override protected[mem] def createEdge(id: Long, from: Long, key: Property, to: Long): GEdge[Any, Any] =
    super.createEdge(id, from, key, to).asInstanceOf[GEdge[Any, Any]]

  private[this] val newValueLock = new Object
  protected[mem] def newValue[T](id: Long, value: T, label: DataType[T]): GValue[T] =
    newValueLock
      .synchronized {
        valueStore.hasId(id).map(_.asInstanceOf[GValue[T]]).getOrElse {
          def _id    = id
          def _value = value
          def _label = label
          val gValue = new _Value[T] with MemValue[T] {
            val id: Long           = _id
            val value: T           = _value
            val label: DataType[T] = _label
            val graph: MemGraph    = thisgraph
          }
          valueStore.store(gValue.asInstanceOf[GValue[_]])
          gValue
        }
      }
      .asInstanceOf[GValue[T]]

  /**
    * delete in-/out-going edges from the resource
    * @param resource
    */
  protected def deleteResource[T <: _Resource[_]](resource: T): Unit = {
    resource.outEMap().foreach {
      case (key, properties) =>
        properties.foreach(edge => edge.to.removeIn(edge))
    }
    resource.inEMap().foreach {
      case (key, properties) =>
        properties.foreach(edge => edge.from.removeOut(edge))
    }
  }

  val computer = DefaultStreamComputer()
  def buildTraversersStream[Start <: ClassType[_], End <: ClassType[_], Steps <: HList, Out](
      traversal: Traversal[Start, End, Steps]): Stream[Out] =
    computer.traverse[Start, End, Steps, Out, this.type](traversal)(thisgraph)

  def buildAsyncTraversersStream[Start <: ClassType[_], End <: ClassType[_], Steps <: HList, Out](
      traversal: Traversal[Start, End, Steps]): Task[Stream[Out]] =
    Task(computer.traverse[Start, End, Steps, Out, this.type](traversal)(thisgraph))

  def toFile(path: String = "defaultname.json", process: (Stream[Resource[_]], String => Unit) => String): Task[Unit] =
    Task {
      import java.io._

      // FileWriter
      val jsonfile = new File(path)
      val bw       = new BufferedWriter(new FileWriter(jsonfile))
      val context = process(nodes(), { value: String =>
        bw.write(value)
        bw.newLine()
      })
      bw.close()
      val contextfile = new File(path + ".context")
      val bwc         = new BufferedWriter(new FileWriter(contextfile))
      bwc.write(context)
      bwc.close()
    }
}
