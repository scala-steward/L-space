package lspace.provider.mem

import lspace.NS
import lspace.datatype.DataType
import monix.eval.Task
import lspace.librarian.traversal._
import lspace.Label.P
import lspace.Label.D
import lspace.provider.mem.store.{MemEdgeStore, MemNodeStore, MemValueStore}
import lspace.provider.transaction.Transaction
import lspace.structure._
import lspace.structure.store.{EdgeStore, NodeStore, ValueStore}
import lspace.structure.util.IdProvider
import monix.execution.atomic.Atomic
import monix.reactive.Observable
import shapeless.{::, HList}

object MemGraph {

//  lazy val default: MemGraph = {
//    //    Graph.graphs += "default" -> MemGraphDefault
//
//  }

  def apply(_iri: String): MemGraph = {
    val graph = new MemDataGraph {
      val iri: String = _iri

      lazy val idProvider: IdProvider = new IdProvider {
        private val id       = Atomic(1000L)
        def next: Task[Long] = Task.now(id.incrementAndGet())
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
      init.runToFuture(lspace.Implicits.Scheduler.global)
    }

    graph
  }

}

trait MemGraph extends Graph {
//  type _Resource[T] = _Resource[T] with MemResource[T]
//  type GNode        = _Node with MemNode //with _Resource[Node]
//  type GEdge[S, E]  = _Edge[S, E] with MemEdge[S, E] //with _Resource[Edge[S, E]]
//  type GValue[T]    = _Value[T] with MemValue[T] //with _Resource[T]
  type GResource[T] = _Resource[T] with MemResource[T]
  type GNode        = _Node with MemNode
  type GEdge[S, E]  = _Edge[S, E] with MemEdge[S, E]
  type GValue[T]    = _Value[T] with MemValue[T]

  def transaction: Transaction = MemTransaction(thisgraph)

  protected[lspace] val nodeStore: MemNodeStore[this.type]   = MemNodeStore("@node", thisgraph)
  protected[lspace] val edgeStore: MemEdgeStore[this.type]   = MemEdgeStore("@edge", thisgraph)
  protected[lspace] val valueStore: MemValueStore[this.type] = MemValueStore("@value", thisgraph)

  protected[mem] val `@idStore`: MemValueStore[this.type] =
    MemValueStore(NS.types.`@id`, thisgraph)

  private[this] val newNodeLock = new Object
  protected[lspace] def newNode(id: Long): GNode = newNodeLock.synchronized {
    nodeStore.cached
      .hasId(id)
      .getOrElse {
        def _id = id
        val node = new _Node with MemNode {
          val id              = _id
          val graph: MemGraph = thisgraph
        }
        nodeStore.cache(node.asInstanceOf[_Node])
        node
      }
      .asInstanceOf[GNode]
  }

  override protected[lspace] def storeNode(node: GNode): Task[Unit] = super.storeNode(node)

  protected[this] val newEdgeLock = new Object
  protected[lspace] def newEdge[S, E](id: Long, from: _Resource[S], key: Property, to: _Resource[E]): GEdge[S, E] =
    newEdgeLock
      .synchronized {
        edgeStore.cached.hasId(id).getOrElse {
          def _id   = id
          def _from = from
          def _key  = key
          def _to   = to
          val edge = new _Edge[S, E] with MemEdge[S, E] {
            val id: Long           = _id
            val from: _Resource[S] = _from
            val key: Property      = _key
            val to: _Resource[E]   = _to
            val graph: MemGraph    = thisgraph
          }
          edgeStore.cache(edge.asInstanceOf[_Edge[_, _]])
          edge
        }
      }
      .asInstanceOf[GEdge[S, E]]

//  protected[mem] def newEdge(id: Long, from: Long, key: Property, to: Long): _Edge[Any, Any] = {
//    val _from = resources
//      .hasId(from)
//      .map(_.asInstanceOf[_Resource[Any]])
//      .getOrElse {
//        throw new Exception(s"cannot create edge, from-resource with id ${from} not found")
//      }
//    val _to =
//      resources
//        .hasId(to)
//        .map(_.asInstanceOf[_Resource[Any]])
//        .getOrElse {
//          throw new Exception(s"cannot create edge, to-resource with id ${to} not found")
//        }
//
//    val edge = createEdge(id, _from, key, _to)
//    edge.asInstanceOf[_Edge[Any, Any]]
//  }

//  override protected[mem] def createEdge(id: Long, from: Long, key: Property, to: Long): _Edge[Any, Any] =
//    super.createEdge(id, from, key, to).asInstanceOf[_Edge[Any, Any]]

  protected[this] val newValueLock = new Object
  protected[lspace] def newValue[T](id: Long, value: T, label: DataType[T]): GValue[T] =
    newValueLock
      .synchronized {
        valueStore.cached.hasId(id).map(_.asInstanceOf[_Value[T]]).getOrElse {
          def _id    = id
          def _value = value
          def _label = label
          val gValue = new _Value[T] with MemValue[T] {
            val id: Long           = _id
            val value: T           = _value
            val label: DataType[T] = _label
            val graph: MemGraph    = thisgraph
          }
          valueStore.cache(gValue.asInstanceOf[valueStore.T])
          gValue
        }
      }
      .asInstanceOf[GValue[T]]

  /**
    * delete in-/out-going edges from the resource
    * @param resource
    */
  protected def deleteResource[T <: _Resource[_]](resource: T): Task[Unit] =
    (Observable.fromIterable(resource.outE()).mapEval(_.remove()) ++ Observable
      .fromIterable(resource.inE())
      .mapEval(_.remove())).completedL
//    Observable.fromIterable(resource.outEMap()).flatMap {
//      case (key, properties) =>
//        Observable
//          .fromIterable(properties)
//          .mapEval(edge =>
//            for {
////              _ <- if ((edge.key == P.`@id` || edge.key == P.`@ids`) && edge.to
////                         .in(P.`@id`, P.`@ids`)
////                         .isEmpty && edge.to
////                         .isInstanceOf[_Value[_]] && edge.to.asInstanceOf[_Value[_]].label == lspace.Label.D.`@string`)
////                `@idStore`.delete(edge.to.asInstanceOf[GValue[_]])
////              else Task.unit
//              _ <- edge.to.removeIn(edge)
//            } yield ())
//    } ++ Observable.fromIterable(resource.inEMap()).flatMap {
//      case (key, properties) =>
//        Observable
//          .fromIterable(properties)
//          .mapEval(edge =>
//            for {
//              _ <- edge.from.removeOut(edge)
//            } yield ())
//    } completedL

  def toFile(path: String = "defaultname.json",
             process: (Observable[Resource[_]], String => Unit) => Task[String]): Task[Unit] =
    Task.defer {
      import java.io._

      // FileWriter
      val jsonfile = new File(path)
      val bw       = new BufferedWriter(new FileWriter(jsonfile))

      for {
        context <- process(nodes(), { value: String =>
          bw.write(value)
          bw.newLine()
        })
      } yield {
        bw.close()

        val contextfile = new File(path + ".context")
        val bwc         = new BufferedWriter(new FileWriter(contextfile))
        bwc.write(context)
        bwc.close()
      }
    }
}
