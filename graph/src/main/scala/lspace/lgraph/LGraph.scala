package lspace.lgraph

import lspace.datatype.DataType
import lspace.lgraph.index.{IndexManager, IndexProvider}
import lspace.lgraph.store._
import lspace.provider.transaction.Transaction
import lspace.structure._
import lspace.structure.util.IdProvider
import monix.eval.Task
import monix.reactive.Observable

object LGraph {

  /**
    *
    * @param cacheLevel fraction of resources to populate the cache, 0 for nothing 1 for everything (if memory allows for it)
    */
  case class Options(cacheLevel: Double = 1.0)
  def apply(storeProvider: StoreProvider,
            indexProvider: IndexProvider,
            options: Options = Options(),
            noinit: Boolean = false): LGraph = {
    val _iri           = storeProvider.iri

    val graph = new LDataGraph {
      val iri: String          = _iri
      private val self: LGraph = this

//      protected lazy val cacheReaper: CacheReaper = CacheReaper(thisgraph)

      override lazy val init: Task[Unit] =
        (for {
          _ <- Task
            .sequence(
              Seq(
                ns.storeManager.init,
                ns.index.storeManager.init,
                storeManager.init,
                index.storeManager.init
              ))
        } yield ()).memoize

      lazy val storeManager: StoreManager[this.type] = storeProvider.dataManager(this)

      lazy val ns: LNSGraph = new LNSGraph {
        val iri: String = _iri + ".ns"

        lazy val graph: LGraph = self

        private lazy val _thisgraph = thisgraph
        lazy val index: LIndexGraph = new LIndexGraph {
          val iri: String = _iri + ".ns" + ".index"

          lazy val graph: LGraph = _thisgraph

          lazy val storeManager: StoreManager[this.type] = storeProvider.nsIndexManager(thisgraph)
          lazy val indexManager: IndexManager[this.type] = indexProvider.nsManager(thisgraph)
        }

        lazy val storeManager: StoreManager[this.type] = storeProvider.nsManager(this)
      }

      lazy val index: LIndexGraph = new LIndexGraph {
        val iri: String = _iri + ".index"

        lazy val graph: LGraph      = self

        lazy val storeManager: StoreManager[this.type] = storeProvider.indexManager(this)
        lazy val indexManager: IndexManager[this.type] = indexProvider.dataManager(this)
      }

      lazy val stateManager: GraphManager[this.type] = storeProvider.stateManager(this)
      lazy val idProvider: IdProvider                = stateManager.idProvider

      override def close(): Task[Unit] =
        for {
//        cacheReaper.kill()
          _ <- super.close()
          _ <- stateManager.close()
        } yield ()
    }
    if (!noinit) graph.init
    graph
  }
}

trait LGraph extends Graph {
  type GResource[T] = _Resource[T] with LResource[T]
  type GNode        = _Node with LNode
  type GEdge[S, E]  = _Edge[S, E] with LEdge[S, E]
  type GValue[T]    = _Value[T] with LValue[T]

  protected[lspace] def storeManager: StoreManager[this.type]

//  protected[lspace] def storeProvider: StoreProvider

  def ns: LNSGraph

  override protected[lspace] lazy val nodeStore: LNodeStore[this.type]   = LNodeStore("@node", thisgraph)
  override protected[lspace] lazy val edgeStore: LEdgeStore[this.type]   = LEdgeStore("@edge", thisgraph)
  override protected[lspace] lazy val valueStore: LValueStore[this.type] = LValueStore("@value", thisgraph)

  object writenode
  protected[lspace] def newNode(id: Long): _Node with LNode = writenode.synchronized {
    nodeStore
      .cachedById(id)
      .getOrElse {
        def _id = id

        val node = new _Node with LNode {
          val id            = _id
          val graph: LGraph = thisgraph
        }
        nodeStore.cache(node.asInstanceOf[_Node])
        node
      }
//      .asInstanceOf[_Node]
  }

  override protected[lspace] def getOrCreateNode(id: Long): Task[GNode] = synchronized {
    for { node <- super.getOrCreateNode(id) } yield {
      val lastaccess = LResource.getLastAccessStamp()
      node._lastoutsync = Some(lastaccess)
      node._lastinsync = Some(lastaccess)
      node
    }
  }

  override protected[lgraph] def storeNode(node: _Node): Task[Unit] = super.storeNode(node)

  object writeedge
  protected[lspace] def newEdge[S, E](id: Long, from: _Resource[S], key: Property, to: _Resource[E]): GEdge[S, E] =
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
            val from: _Resource[S] = _from
            val key: Property      = _key
            val to: _Resource[E]   = _to
            val graph: LGraph      = thisgraph
          }
          edgeStore.cache(edge.asInstanceOf[_Edge[_, _]])
          edge
        }
        .asInstanceOf[GEdge[S, E]]
    }

//  protected[lgraph] def newEdge(id: Long, from: Long, key: Property, to: Long): _Edge[Any, Any] = {
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

  override protected[lspace] def createEdge[S, E](id: Long,
                                                  from: _Resource[S],
                                                  key: Property,
                                                  to: _Resource[E]): Task[GEdge[S, E]] =
    for { edge <- super.createEdge(id, from, key, to) } yield {
      val lastaccess = LResource.getLastAccessStamp()
      edge._lastoutsync = Some(lastaccess)
      edge._lastinsync = Some(lastaccess)
      edge
    }

  object writevalue
  protected[lspace] def newValue[T](id: Long, value: T, label: DataType[T]): GValue[T] =
    writevalue
      .synchronized {
        valueStore.cachedById(id).map(_.asInstanceOf[_Value[T]]).getOrElse {
          def _id    = id
          def _value = value
          def _label = label
          val gValue = new _Value[T] with LValue[T] {
            val id: Long           = _id
            val value: T           = _value
            val label: DataType[T] = _label
            val graph: LGraph      = thisgraph
          }
          valueStore.cache(gValue.asInstanceOf[_Value[_]])
          gValue
        }
      }
      .asInstanceOf[GValue[T]]

  override protected[lspace] def createValue[T](id: Long, value: T, dt: DataType[T]): Task[GValue[T]] =
    for { rv <- super.createValue(id, value, dt) } yield {
      val lastaccess = LResource.getLastAccessStamp()
      rv._lastoutsync = Some(lastaccess)
      rv._lastinsync = Some(lastaccess)
      rv
    }

  protected[lspace] def deleteResource[T <: _Resource[_]](resource: T): Task[Unit] =
    (Observable.fromIterable(resource.asInstanceOf[LResource[Any]].outEMap()).flatMap {
      case (_, properties) =>
        Observable.fromIterable(properties).mapEval(edge => edge.to.removeIn(edge))
    } ++ Observable.fromIterable(resource.asInstanceOf[LResource[Any]].inEMap()).flatMap {
      case (_, properties) =>
        Observable.fromIterable(properties).mapEval(edge => edge.from.removeOut(edge))
    }).completedL

  override def transaction: Transaction = LTransaction(thisgraph)

//  override def persist: CancelableFuture[Unit] = storeManager.persist.runToFuture(monix.execution.Scheduler.global)

  override def purge: Task[Unit] =
    for {
      _ <- super.purge
      _ <- storeManager.purge
    } yield ()

  override def close(): Task[Unit] =
    for {
    _ <- super.close()
    _ <- storeManager.close()
    } yield ()
}
