package lspace.lgraph

import java.time.Instant

import lspace.lgraph.index.{IndexManager, IndexProvider, LIndex}
import lspace.lgraph.store._
import lspace.lgraph.util.CacheReaper
import lspace.librarian.process.computer.DefaultStreamComputer
import monix.eval.Task
import lspace.librarian.process.traversal.Traversal
import lspace.librarian.provider.transaction.Transaction
import lspace.librarian.structure._
import lspace.librarian.structure.util.IdProvider
import shapeless.HList

object LGraph {
  def apply(storeProvider: StoreProvider, indexProvider: IndexProvider): LGraph = {
    val _iri           = storeProvider.iri
    val _storeProvider = storeProvider
    val _indexProvider = indexProvider

    val graph = new LDataGraph {
      val iri: String  = _iri
      private val self = this

      protected lazy val cacheReaper: CacheReaper = CacheReaper(thisgraph)

      override def init(): Unit = {
        super.init()
        cacheReaper
      }

      lazy val storeManager: StoreManager[this.type] = storeProvider.dataManager(this)

      def ns: NameSpaceGraph = new LNSGraph {
        val iri: String = _iri + ".ns"

        lazy val graph: LGraph = self

        private val _thisgraph = thisgraph
        lazy val index: LIndexGraph = new LIndexGraph {
          def iri: String = _iri + ".ns" + ".index"

          lazy val graph: LGraph      = _thisgraph
          lazy val index: LIndexGraph = this

          lazy val storeManager: StoreManager[this.type] = storeProvider.nsIndexManager(this)
          lazy val indexManager: IndexManager[this.type] = indexProvider.nsIndexManager(this)
        }

        lazy val storeManager: StoreManager[this.type] = storeProvider.nsManager(this)
      }

      def index: IndexGraph = new LIndexGraph {
        def iri: String = _iri + ".index"

        lazy val graph: LGraph = self
        private val _thisgraph = thisgraph

        lazy val index: LIndexGraph = new LIndexGraph {
          def iri: String = _iri + ".index" + ".index"

          lazy val graph: LGraph      = _thisgraph
          lazy val index: LIndexGraph = this

          lazy val storeManager: StoreManager[this.type] = storeProvider.indexIndexManager(this)
          lazy val indexManager: IndexManager[this.type] = indexProvider.indexIndexManager(this)
        }

        lazy val storeManager: StoreManager[this.type] = storeProvider.indexManager(this)
        lazy val indexManager: IndexManager[this.type] = indexProvider.indexManager(this)
      }

      val stateManager: GraphManager[this.type] = storeProvider.stateManager(this)
      lazy val idProvider: IdProvider           = stateManager.idProvider

      override def close(): Unit = {
        cacheReaper.kill()
        super.close()
        stateManager.close()
      }
    }
    graph.init()
    graph
  }
//  def apply(_iri: String, _idProvider: IdProvider, _ns: LNSGraph, _index: LIndexGraph)
}

trait LGraph extends Graph {
  type GNode       = _Node with LNode
  type GEdge[S, E] = _Edge[S, E] with LEdge[S, E]
  type GValue[T]   = _Value[T] with LValue[T]
//  def createStoreManager[G <: LGraph]: G => StoreManager[G]
//  def stateManager: GraphManager[this.type]
  protected[lgraph] def storeManager: StoreManager[this.type]

  override protected[lgraph] val nodeStore: LNodeStore[this.type]   = LNodeStore("@node", thisgraph)
  override protected[lgraph] val edgeStore: LEdgeStore[this.type]   = LEdgeStore("@edge", thisgraph)
  override protected[lgraph] val valueStore: LValueStore[this.type] = LValueStore("@value", thisgraph)

  object writenode
  protected[lgraph] def newNode(id: Long): GNode = writenode.synchronized {
    nodeStore
      .cachedById(id)
      .getOrElse {
        def _id = id

        val node = new _Node with LNode {
          val id            = _id
          val graph: LGraph = thisgraph
        }
//        nodeStore.cache(node.asInstanceOf[GNode])
        node
      }
      .asInstanceOf[GNode]
  }

  override protected def getOrCreateNode(id: Long): GNode = synchronized {
    val node       = super.getOrCreateNode(id)
    val lastaccess = LResource.getLastAccessStamp()
    node._lastoutsync = Some(lastaccess)
    node._lastinsync = Some(lastaccess)
    node
  }

  override protected[lgraph] def storeNode(node: GNode): Unit = super.storeNode(node)

  object writeedge
  protected[lgraph] def newEdge[S, E](id: Long, from: GResource[S], key: Property, to: GResource[E]): GEdge[S, E] =
    writeedge.synchronized {
      edgeStore
        .cachedById(id)
        .getOrElse {
          def _id = id

          def _from = from

          def _key = key

          def _to = to

          val edge = new _Edge[S, E] with LEdge[S, E] {
            val id: Long           = _id
            val from: GResource[S] = _from
            val key: Property      = _key
            val to: GResource[E]   = _to
            val graph: LGraph      = thisgraph
          }
//          edgeStore.cache(edge.asInstanceOf[GEdge[_, _]])
          edge
        }
        .asInstanceOf[GEdge[S, E]]
    }

  protected[lgraph] def newEdge(id: Long, from: Long, key: Property, to: Long): GEdge[Any, Any] = {
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

  override protected def createEdge[S, E](id: Long,
                                          from: GResource[S],
                                          key: Property,
                                          to: GResource[E]): GEdge[S, E] = {
    val edge       = super.createEdge(id, from, key, to)
    val lastaccess = LResource.getLastAccessStamp()
    edge._lastoutsync = Some(lastaccess)
    edge._lastinsync = Some(lastaccess)
    edge
  }

  object writevalue
  protected[lgraph] def newValue[T](id: Long, value: T, label: DataType[T]): GValue[T] =
    writevalue
      .synchronized {
        valueStore.cachedById(id).map(_.asInstanceOf[GValue[T]]).getOrElse {
          def _id    = id
          def _value = value
          def _label = label
          val gValue = new _Value[T] with LValue[T] {
            val id: Long           = _id
            val value: T           = _value
            val label: DataType[T] = _label
            val graph: LGraph      = thisgraph
          }
//          valueStore.cache(gValue.asInstanceOf[GValue[_]])
          gValue
        }
      }
      .asInstanceOf[GValue[T]]

  override protected def createValue[T](id: Long, value: T, dt: DataType[T]): GValue[T] = {
    val rv         = super.createValue(id, value, dt)
    val lastaccess = LResource.getLastAccessStamp()
    rv._lastoutsync = Some(lastaccess)
    rv._lastinsync = Some(lastaccess)
    rv
  }

  protected def deleteResource[T <: _Resource[_]](resource: T): Unit = {
    resource.asInstanceOf[LResource[Any]].outEMap().foreach {
      case (key, properties) =>
        properties.foreach(edge => edge.to.removeIn(edge))
    }
    resource.asInstanceOf[LResource[Any]].inEMap().foreach {
      case (key, properties) =>
        properties.foreach(edge => edge.from.removeOut(edge))
    }
  }

  override def transaction: Transaction = LTransaction(thisgraph)

  val computer = DefaultStreamComputer()
  def buildTraversersStream[Start <: ClassType[_], End <: ClassType[_], Steps <: HList, Out](
      traversal: Traversal[Start, End, Steps])(ct: ClassType[_]): Stream[Out] =
    computer.traverse[Start, End, Steps, Out, this.type](traversal)(thisgraph)

  def buildAsyncTraversersStream[Start <: ClassType[_], End <: ClassType[_], Steps <: HList, Out](
      traversal: Traversal[Start, End, Steps])(ct: ClassType[_]): Task[Stream[Out]] =
    Task(computer.traverse[Start, End, Steps, Out, this.type](traversal)(thisgraph))

  override def close(): Unit = {
    super.close()
    storeManager.close()
  }
}
