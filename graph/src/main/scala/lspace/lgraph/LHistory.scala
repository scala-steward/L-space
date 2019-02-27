package lspace.lgraph

import lspace.lgraph.index.{IndexManager, IndexProvider}
import lspace.lgraph.store.{StoreManager, StoreProvider}
import lspace.structure.util.IdProvider
import lspace.structure.{DataGraph, History, NameSpaceGraph}
import monix.execution.CancelableFuture

object LHistory {
  def apply(iri: String, storeProvider: StoreProvider, indexProvider: IndexProvider): LGraph = {
    val _iri           = iri
    val _storeProvider = storeProvider
    val _indexProvider = indexProvider

    val graph = new LHistory {
      val iri: String  = _iri
      private val self = this

      lazy val storeManager: StoreManager[this.type] = storeProvider.dataManager(this)

      lazy val ns: LNSGraph = new LNSGraph {
        def iri: String = _iri + ".ns"

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

      lazy val index: LIndexGraph = new LIndexGraph {
        def iri: String = _iri + ".index"

        lazy val graph: LGraph = self
        private val _thisgraph = thisgraph

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
      override def idProvider: IdProvider       = stateManager.idProvider

      override def close(): CancelableFuture[Unit] = CancelableFuture.unit
//      {
//        super
//          .close()
//          .flatMap { u =>
//            storeManager.close()
//          }(monix.execution.Scheduler.global)
//      }
    }
    graph.init
    graph
  }
}

trait LHistory extends LDataGraph with History {}
