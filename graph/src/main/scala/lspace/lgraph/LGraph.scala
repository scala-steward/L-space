package lspace.lgraph

import lspace.lgraph.index.{IndexManager, IndexProvider}
import lspace.lgraph.store._
import lspace.lgraph.util.CacheReaper
import lspace.datatype.DataType
import monix.eval.Task
import lspace.librarian.traversal.Traversal
import lspace.provider.transaction.Transaction
import lspace.structure._
import lspace.structure.util.IdProvider
import monix.execution.{Cancelable, CancelableFuture}
import shapeless.HList

object LGraph {

  /**
    *
    * @param cacheLevel fraction of resources to populate the cache, 0 for nothing 1 for everything (if memory allows for it)
    */
  case class Options(cacheLevel: Double = 1.0)
  def apply(storeProvider: StoreProvider, indexProvider: IndexProvider, options: Options = Options()): LGraph = {
    val _iri           = storeProvider.iri
    val _storeProvider = storeProvider
    val _indexProvider = indexProvider

    val graph = new LDataGraph {
      val iri: String  = _iri
      private val self = this

//      protected lazy val cacheReaper: CacheReaper = CacheReaper(thisgraph)

      override lazy val init: CancelableFuture[Unit] =
        Task
          .sequence(
            Seq(
              ns.storeManager.init,
              ns.index.storeManager.init,
              storeManager.init,
              index.storeManager.init
            ))
          .foreachL(f => Task.unit)
          .memoize
          .runToFuture(monix.execution.Scheduler.global)

      lazy val storeManager: StoreManager[this.type] = storeProvider.dataManager(this)

      lazy val ns: LNSGraph = new LNSGraph {
        val iri: String = _iri + ".ns"

        lazy val graph: LGraph = self

//        override lazy val init: CancelableFuture[Unit] =
//          Task
//            .sequence(
//              Seq(
//                Task.fromFuture(storeManager.init),
//                Task.fromFuture(index.init)
//              ))
//            .foreachL(f => Task.unit)
//            .memoize
//            .runToFuture(monix.execution.Scheduler.global)

        private lazy val _thisgraph = thisgraph
        lazy val index: LIndexGraph = new LIndexGraph {
          val iri: String = _iri + ".ns" + ".index"

//          override lazy val init: CancelableFuture[Unit] =
//            Task
//              .sequence(
//                Seq(
//                  Task.fromFuture(storeManager.init)
//                ))
//              .foreachL(f => Task.unit)
//              .memoize
//              .runToFuture(monix.execution.Scheduler.global)

          lazy val graph: LGraph      = _thisgraph
          lazy val index: LIndexGraph = thisgraph

          lazy val storeManager: StoreManager[this.type] = storeProvider.nsIndexManager(thisgraph)
          lazy val indexManager: IndexManager[this.type] = indexProvider.nsIndexManager(thisgraph)
        }

        lazy val storeManager: StoreManager[this.type] = storeProvider.nsManager(this)
      }

      lazy val index: LIndexGraph = new LIndexGraph {
        val iri: String = _iri + ".index"

        lazy val graph: LGraph      = self
        private lazy val _thisgraph = thisgraph

//        lazy val index: LIndexGraph = new LIndexGraph {
//          def iri: String = _iri + ".index" + ".index"
//
//          lazy val graph: LGraph      = _thisgraph
//          lazy val index: LIndexGraph = this
//
//          lazy val storeManager: StoreManager[this.type] = storeProvider.indexIndexManager(this)
//          lazy val indexManager: IndexManager[this.type] = indexProvider.indexIndexManager(this)
//        }

        lazy val storeManager: StoreManager[this.type] = storeProvider.indexManager(this)
        lazy val indexManager: IndexManager[this.type] = indexProvider.indexManager(this)
      }

      val stateManager: GraphManager[this.type] = storeProvider.stateManager(this)
      lazy val idProvider: IdProvider           = stateManager.idProvider

      override def close(): CancelableFuture[Unit] = {
//        cacheReaper.kill()
        super.close()
        stateManager.close()
      }
    }
    graph.init
    graph
  }
}

trait LGraph extends Graph {
  type GNode       = _Node with LNode
  type GEdge[S, E] = _Edge[S, E] with LEdge[S, E]
  type GValue[T]   = _Value[T] with LValue[T]

  protected[lgraph] def storeManager: StoreManager[this.type]

  def ns: LNSGraph

  override protected[lgraph] lazy val nodeStore: LNodeStore[this.type]   = LNodeStore("@node", thisgraph)
  override protected[lgraph] lazy val edgeStore: LEdgeStore[this.type]   = LEdgeStore("@edge", thisgraph)
  override protected[lgraph] lazy val valueStore: LValueStore[this.type] = LValueStore("@value", thisgraph)

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

  override protected[lspace] def getOrCreateNode(id: Long): GNode = synchronized {
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

//  override def persist: CancelableFuture[Unit] = storeManager.persist.runToFuture(monix.execution.Scheduler.global)

  override def close(): CancelableFuture[Unit] = {
    super
      .close()
      .flatMap { u =>
        storeManager.close().runToFuture(monix.execution.Scheduler.global)
      }(monix.execution.Scheduler.global)
  }
}
